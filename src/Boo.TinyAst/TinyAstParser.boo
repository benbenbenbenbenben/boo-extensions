namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.OMeta.Parser
import Boo.Lang.Compiler
import Boo.Lang.Compiler.Ast
import System.Globalization

enum BracketType:
	QQ
	Parenthesis
	Square
	Curly

ast Form = \
	Identifier(Name as string, IsKeyword as bool, IsSymbol as bool) | \
	Brackets(Form, Kind as BracketType)  |\
	Literal(Value as object, astLiteral as LiteralExpression) |\
	Infix(Operator as Identifier, Left, Right) |\
	Prefix(Operator, Operand, IsPostfix as bool) |\
	Tuple(Forms as (Form)) |\
	Pair(Left, Right, IsMultiline as bool, Doc as string) |\
	Block(Forms as (Form))

macro infix:
	l, op, r = infix.Arguments
	return ExpressionStatement([| $l = ((( $l >> l and (not l isa Identifier or not (l as Identifier).IsKeyword), 
				(~KW >> nkw | ""), (~ID >> nid | ""), $op >> op, 
				$r >> r) 
				^ Infix(Identifier(tokenValue(op), nkw is null, (not nkw is null ) and (not nid is null)), l, r)) | $r) |])
	
macro infixr:
	l, op, r = infixr.Arguments
	return ExpressionStatement([| $l = ((($r >> l, $op >> op, $l >> r) ^ Infix(Identifier(tokenValue(op), false, false), l, r)) | $r) |])
	
macro prefix:
	rule, op, next = prefix.Arguments
	return ExpressionStatement([| $rule = ($op >> op, $rule >> e) ^ Prefix(Identifier(tokenValue(op), false, false), e, false) | $next |])

macro prefix_keyword:
	rule, op, next = prefix_keyword.Arguments
	return ExpressionStatement([| $rule = ($op >> op, $rule >> e) ^ Prefix(Identifier(tokenValue(op), true, false), e, false) | $next |])

macro prefix_symbol:
	rule, op, next = prefix_symbol.Arguments
	return ExpressionStatement([| $rule = ($op >> op, $rule >> e) ^ Prefix(Identifier(tokenValue(op), false, true), e, false) | $next |])


ometa TinyAstParser < WhitespaceSensitiveTokenizer:
	tokens:
		at = "@"
		qq_begin = "[|"
		qq_end = "|]"
		splice_begin = "$"
		equality = "=="
		inequality = "!="
		assign = "="
		assign_inplace = "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" | "<<=" | ">>="	
		xor = "^"		
		increment = "++"
		decrement = "--"		
		plus = "+"
		minus = "-"
		exponentiation = "**"
		star = "*"
		division = "/"
		modulus = "%"
		ones_complement = "~"		
		bitwise_shift_left = "<<"
		bitwise_shift_right = ">>"		
		greater_than_eq = ">="
		greater_than = ">"
		less_than_eq = "<="
		less_than = "<"
		bitwise_and = "&"
		bitwise_or = "|"		
		colon = ":"
		semicolon = ";"
		dot = "."
		comma = ","
		kw = (keywords >> value, ~(letter | digit | '_')) ^ value
		tdq = '"""'
		dq = '"'
		sq = "'"
		id = ((letter | '_') >> p, --(letter | digit | '_') >> s) ^ makeString(p, s)
		
		hexnum = ("0x", ++(hex_digit | digit) >> ds) ^ makeString(ds)
		num = ++digit
		
		lparen = "(", enterWhitespaceAgnosticRegion
		rparen = ")", leaveWhitespaceAgnosticRegion
		lbrack = "[", enterWhitespaceAgnosticRegion
		rbrack = "]", leaveWhitespaceAgnosticRegion
		lbrace = "{", enterWhitespaceAgnosticRegion
		rbrace = "}", leaveWhitespaceAgnosticRegion

	keywords "and", "as", "cast", "def", "from", "import", "if", "in", "namespace", "not", "or", "for", "isa", "is", "return"
	keyword[expected] = ((KW >> t) and (expected is tokenValue(t))) ^ t

	hex_digit = _ >> c as char and ((c >= char('a') and c <= char('f')) or (c >= char('A') and c <= char('F'))) 
	sqs_esc =  '\\', ( sesc | '\'' )
	dqs_esc = '\\', ( sesc | '"' | '$')
	sesc =  ("r" ^ "\r") | ("n" ^ "\n") | ("t" ^ "\t") | ("a" ^ "\a") | ("b" ^ "\b") | ("f" ^ "\f") \
		| ("0" ^ "\0") | ("\\" ^ "\\") \
		| ("u", (hex_digit >> h1, hex_digit >> h2, hex_digit >> h3, hex_digit >> h4) ^ getUnicodeChar([h1, h2, h3, h4]))

	sqs = (SQ, --( sqs_esc | (~('\'' | '\\' | '\r' | '\n'), _)) >> s, SQ) ^ makeString(s)		
	dqs = (DQ, --( dqs_esc | (~('"' | '\\' | '\r' | '\n'), _)) >> s, DQ) ^ makeString(s)

	qualified_name = (ID >> qualifier, --((DOT, id >> n) ^ n) >> suffix)^ buildQName(qualifier, suffix)
	
	tqs = (TDQ, --(~tdq, ( (('\\', '$') ^ '$')| _)) >> s, TDQ) ^ makeString(s)
	eol = ++EOL | ~_
	
	space = line_continuation | multi_line_comment | line_comment | super
	
	empty_line = ending_spaces, newline	
	
	ending_spaces = --end_space >> value ^ value
	
	end_space =  semicolon | space
	
	line_continuation = "\\", newline
	multi_line_comment = "/*", --(~"*/", (multi_line_comment | _)), "*/"
	line_comment = ('#' | "//"), --(~newline, _)
	
	namespace_declaration = (NAMESPACE, qualified_name)
	import_declaration = ( (IMPORT, qualified_name >> qn), (((FROM, (dqs | sqs | qualified_name)) | "") >> assembly), ( (AS, ID) | "") >> alias, eol) ^ newImport(qn, assembly, alias)

	module = (
		--EOL,
		((tqs >> s , EOL) | ""),
		--EOL,
		((namespace_declaration >> ns, eol) | ""),
		--import_declaration >> ids,
		((block >> b ^ [newMacroStatement(b)]) >> forms | ""),
		--EOL
	) ^ newModule(ns, s, ids, [], forms)

	form = multi_line_pair | single_line_form
	
	single_line_form = (single_line_pair >> p and ( (p isa Pair) and (not (p as Pair).Right isa Tuple) and (not(p as Pair).Left isa Tuple) )) | inline_block2  | infix_operator

	form_stmt = --(--SEMICOLON, eol), ((multi_line_pair >> f) | (single_line_form >> f, ( (--SEMICOLON, eol)| ++SEMICOLON))) ^ f

	block = (++(form_stmt) >> forms) ^ newBlock(forms)



	single_line_pair = (single_line_pair_prescan >> p and (p isa Pair)) ^ p
	single_line_pair_prescan = (single_line_pair_prescan >> left, COLON, single_line_form >> right ^ Pair(left, right, false, null)) | infix_operator

	multi_line_pair = (multi_line_pair_prescan >> p and ((p isa Pair) and (p as Pair).IsMultiline)) ^ p
	multi_line_pair_prescan = (multi_line_pair_prescan >> left, (begin_block_with_doc >> doc | begin_block), block >> right, DEDENT ^ Pair(left, right, true, doc)) | single_line_form

	begin_block = COLON, INDENT

	begin_block_with_doc = (COLON,
		--EOL,
		tqs >> s,
		INDENT) ^ s	
	
	infix_operator = inline_block
	
	inline_block = (inline_block >> t, SEMICOLON, prefix_expression >> last ^ newBlock(t, last)) | prefix_expression
	inline_block2 = (inline_block2 >> t, SEMICOLON, prefix_expression2 >> last ^ newBlock(t, last)) | prefix_expression2

	prefix_expression = (tuple >> op, prefix_expression >> e ^ Prefix(op, e, false)) | tuple #right infix
	prefix_expression2 = (tuple2 >> op, prefix_expression2 >> e ^ Prefix(op, e, false)) | tuple2 #right infix	
	
	tuple = (tuple >> t, COMMA, (brakets_prefix >> last | "") ^ newTuple(t, last)) | brakets_prefix
	tuple2 = (tuple2 >> t, COMMA, (pair >> last | "") ^ newTuple(t, last)) | pair	
	
	pair = (pair >> left, COLON, brakets_prefix >> right ^ Pair(left, right, false, null)) | brakets_prefix
	
	brakets_prefix = ( (brakets_prefix >> op and (op isa Brackets)), assignment >> e ^ Prefix(op, e, false)) | assignment
	
	infixr assignment, (ASSIGN | ASSIGN_INPLACE), or_expression
	infix or_expression, OR, and_expression
	infix and_expression, AND, not_expression	
	prefix_keyword not_expression, NOT, membership_expression
	#prefix_symbol explode_operator, STAR , membership_expression //(possible conflict with high_priority_prefix)

	infix membership_expression, (IN | ((NOT, IN) ^ makeToken("not in"))), identity_test_expression
	
	infix identity_test_expression, (((IS, NOT) ^ makeToken("is not")) | IS), isa_expression
	
	infix isa_expression, ISA, comparison
	
	infix comparison, (EQUALITY | INEQUALITY | GREATER_THAN | GREATER_THAN_EQ | LESS_THAN | LESS_THAN_EQ), bitwise_or_expression	
	
	infix bitwise_or_expression, BITWISE_OR, bitwise_xor_expression	
	infix bitwise_xor_expression, XOR, bitwise_and_expression	
	infix bitwise_and_expression, BITWISE_AND, term

	infix term, (PLUS | MINUS), factor
	infix factor, (STAR | DIVISION | MODULUS), bitwise_shift_expression	
	infix bitwise_shift_expression, (BITWISE_SHIFT_LEFT | BITWISE_SHIFT_RIGHT), signalled_expression
	prefix_symbol signalled_expression, (MINUS | INCREMENT | DECREMENT), ones_complement_expression
	prefix_symbol ones_complement_expression, ONES_COMPLEMENT, exponentiation_expression
	infix exponentiation_expression, EXPONENTIATION, as_operator
	infixr as_operator, AS, cast_operator
	
	infix cast_operator, CAST, postfix_operator
	
	postfix_operator  =  (postfix_operator >> e, (INCREMENT | DECREMENT) >> op ^ Prefix(Identifier(tokenValue(op), false, true), e, true)) | member_reference
	infix member_reference, DOT, splice
	prefix_symbol splice, SPLICE_BEGIN, at_operator
	prefix at_operator, AT, high_priority_prefix

	high_priority_prefix = ( (STAR | (~~DOT, ~atom /*not float, ex: .001*/, DOT)) >> op, prefix_of_brackets >> e ^ Prefix(Identifier(tokenValue(op), false, true), e, false)) | prefix_of_brackets

	prefix_of_brackets = (
							
							(prefix_of_brackets >> op and 
								(
									#Identifier + Brackets
									(op isa Identifier and not (op as Identifier).IsKeyword)
									#Bracket sticks to bracket
									or (op isa Prefix and (op as Prefix).Operand isa Brackets)
								)
							), 	exp_in_brackets >> e ^ Prefix(op, e, false)
						 ) | \
						 (
						 	#Keyword + Brackets. Ex: def()
						 	#Keyword before brackets has lower priority then infix
						 	#Ex: for (n, a) in zip(names, attributes) =>  for ((n, a) in zip(names, attributes))
							(prefix_of_brackets >> op and (op isa Identifier and (op as Identifier).IsKeyword)), ~(tuple >> io and (io isa Infix)), 
								exp_in_brackets >> e ^ Prefix(op, e, false)
						 ) | \
						 atom
	
	atom = exp_in_brackets | identifier | literal

	identifier = (ID >> s ^ Identifier(tokenValue(s), false, false)) | (KW >> s ^ Identifier(tokenValue(s), true, false))
	
	exp_in_brackets = paren_brackets | qq_brackets | square_brackets | curly_brackets

	paren_brackets = (LPAREN, ( form | "" ) >> f, RPAREN) ^  Brackets(f, BracketType.Parenthesis)

	qq_brackets = ((QQ_BEGIN, INDENT, block >> f, DEDENT, QQ_END) | (QQ_BEGIN, form >> f, QQ_END)) ^ Brackets(f, BracketType.QQ)
	
	square_brackets = (LBRACK, ( form | "") >> f, RBRACK) ^ Brackets(f, BracketType.Square)
	
	curly_brackets = (LBRACE, ( form | "") >> f, RBRACE) ^ Brackets(f, BracketType.Curly)
	
	literal = float | integer | string_literal 
	
	integer = (
		((MINUS | "") >> sign, NUM >> n and (IsValidLong(sign, n)), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.AllowLeadingSign, suffix)) \
		| ((MINUS | "") >> sign, (HEXNUM >> n and (IsValidHexLong(sign, n))), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.HexNumber, suffix))
	) >> i ^ Literal((i as IntegerLiteralExpression).Value, i)
	
	string_literal = (sqs | dqs) >> s ^ Literal(s, newStringLiteral(s))

	float = ( (fractional_constant >> n, (exponent_part | "") >> e , floating_suffix ) ^ newFloat(makeString(n,e))) | ((NUM >> n, exponent_part >> e, floating_suffix)  ^ newFloat(makeString(tokenValue(n),e)))

	fractional_constant = ((NUM >> a , DOT , NUM >> b) ^ makeString(tokenValue(a),".",tokenValue(b))) | ( (DOT , NUM >> b) ^ makeString(".",tokenValue(b)) ) | ( (NUM >> a , DOT, ~(ID)) ^ makeString(tokenValue(a), ".") )
    
	exponent_part = ( ("e" | "E") , exposignopt >> e , NUM >> d ) ^ makeString("e", e, tokenValue(d))

	exposignopt = ( (PLUS | MINUS) >> e ^ makeString(tokenValue(e)) ) | ""
	
	floating_suffix = "f" | "F" | ""

def newFloat(t):
	value = double.Parse(t)
	return Literal(value, DoubleLiteralExpression(Value: value))

def newTuple(f):
	if f isa Tuple: return f
	return Tuple(array(Form,f as List))

def newTuple(t, last as Form):
	if t isa Tuple:
		tu = t as Tuple
		return t if last is null
		return Tuple(tu.Forms + (last,))
	else:
		return Tuple(array(Form,[t])) if last is null
		return Tuple(array(Form,[t,last]))

def newBlock(t as Form, last as Form):
	return Block((t as Block).Forms + (last,)) if t isa Block
	return Block((t, last))

def newMacroStatement(data):
	m = MacroStatement("tinyAst")
	m.Annotate("tinyAst", data)
	return m
	
def newBlock(forms):
	list = []
	for form in forms:
		if form isa Block:
			for item in (form as Block).Forms:
				list.Add(item)
		else:
			list.Add(form)		
	return Block(array(Form, list))
	
def newModule(ns as string, doc, imports, members, stmts):
	m = Module(Documentation: doc)
	
	if ns is not null:
		m.Namespace = NamespaceDeclaration(ns)
	
	for item in imports: m.Imports.Add(item)
	for member in flatten(members):
		if member isa Ast.Attribute:
			m.AssemblyAttributes.Add(member)
		elif member isa MacroStatement:
			m.Globals.Add(member as Statement)
		else:
			m.Members.Add(member)
	for stmt as Statement in flatten(stmts): m.Globals.Add(stmt)
	return m

	