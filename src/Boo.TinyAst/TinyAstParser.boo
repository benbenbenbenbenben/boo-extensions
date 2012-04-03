namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.OMeta.Parser
import Boo.Lang.Compiler
import Boo.Lang.Compiler.Ast
import System.Globalization

enum BracketsType:
	QQ
	Parenthesis
	Square
	Curly

ast Form = \
	Identifier(Name as string, IsKeyword as bool, IsSymbol as bool) | \
	Brackets(Form, Type as BracketsType)  |\
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
		closure_separator = "=>"
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
		

	keywords "and", "as", "cast", "class", "def", "elif", "else", "from", "interface", "import", "if", "in", "namespace", "not", "of", "or", "for", "isa" \
				, "is", "return", "raise", "unless", "while"
//	keywords "abstract", "and", "as", "callable", "cast", "class", "constructor", "def", "do", "elif", "else", \
//		"ensure", "enum", "event", "except", "failure", "final", "for", "from", "goto", "if", "import", \
//		"interface", "internal", "in", "isa", "is", "namespace", "new", "not", "null", "of", "or", "override", \
//		"pass", "private", "protected", "public", "raise", "return", "self", "static", "struct", "super", \
//		"then", "transient", "try", "typeof", "unless", "virtual", "while", "yield"	
	
	keyword[expected] = ((KW >> t) and (expected is tokenValue(t))) ^ t

	hex_digit = _ >> c as char and ((c >= char('a') and c <= char('f')) or (c >= char('A') and c <= char('F'))) 
	sqs_esc =  '\\', ( sesc | '\'' )
	dqs_esc = '\\', ( sesc | '"' | '$')
	sesc =  ("r" ^ "\r") | ("n" ^ "\n") | ("t" ^ "\t") | ("a" ^ "\a") | ("b" ^ "\b") | ("f" ^ "\f") \
		| ("0" ^ "\0") | ("\\" ^ "\\") \
		| ("u", (hex_digit >> h1, hex_digit >> h2, hex_digit >> h3, hex_digit >> h4) ^ getUnicodeChar([h1, h2, h3, h4]))

	sqs = (SQ, --( sqs_esc | (~('\'' | '\\' | '\r' | '\n'), _)) >> s, SQ) ^ makeString(s)		
	dqs = (DQ, --( dqs_esc | (~('"' | '\\' | '\r' | '\n'), _)) >> s, DQ) ^ makeString(s)

	single_quote_string = (SQ, --( ('\\\'' ^ '\\\'') | (~('\'' | '\r' | '\n'), _)) >> s, SQ) ^ makeString(["'", s, "'"])		
	double_quote_string = (DQ, --( ("\\\"" ^ "\\\"") | (~('"' | '\r' | '\n'), _)) >> s, DQ) ^ makeString(['"', s, '"'])

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

	form_stmt = --(--SEMICOLON, eol), form >> f, ((--SEMICOLON, eol) | ++SEMICOLON)  ^ f

	block = (++( form_stmt) >> forms) ^ newBlock(forms)

	begin_block = COLON, INDENT

	begin_block_with_doc = (COLON,
		--EOL,
		tqs >> s,
		INDENT) ^ s	

	form = closure_separation
	
	infixr closure_separation, CLOSURE_SEPARATOR, inline_block
	
	inline_block = (inline_block >> t, SEMICOLON, prefix_expression >> last ^ newBlock(t, last)) | prefix_expression
	
	prefix_expression = (
							(tuple | brakets_prefix) >> op
							,(
								(
									(
										("" and (op isa Identifier and ((op as Identifier).IsKeyword and not (op as Identifier).Name == "of")), reset_lp_tuple) 
										| ("" and (op isa Identifier and (not (op as Identifier).IsKeyword or (op as Identifier).Name == "of")), enter_lp_tuple) 
										| ""
									)
									, ++prefix_expression >> e
									//, (("" and (op isa Identifier and not (op as Identifier).IsKeyword), leave_tuple) | "")
								) 
							) ^ getRight(op, e)
						) \
						| pair | single_line_pair | tuple | pair_expression #right infix

	pair = (tuple | brakets_prefix) >> left and ((not left isa Infix) or not inTuple(input)), (begin_block_with_doc >> doc | begin_block), block >> right, DEDENT, prepend_eol ^ Pair(left, right, true, doc)

	single_line_pair = (brakets_prefix >> left, COLON, prefix_expression >> right ^ Pair(left, right, false, null))
	
	//tuple - comma has priority higher than prefix but lower than infix
	tuple = ( 	in_lp_tuple # enter_lp_tuple should be called before entering lp_tule
//				, ~in_tuple
				, enter_tuple
				, pair_expression >> head, --(COMMA, pair_expression) >> tail, (COMMA | ("" and (len(tail)> 0))), leave_tuple ) ^ newTuple(head, tail)
	
	def newTuple(head, tail):
		return Tuple(array(Form,[head])) if tail is null
		return Tuple(array(Form,[head,tail])) if not tail isa List
		return Tuple(array(Form,[head]+ (tail as List)))
	
	pair_expression = (brakets_prefix >> left, COLON, brakets_prefix >> right ^ Pair(left, right, false, null)) | brakets_prefix
	
	brakets_prefix = ( (brakets_prefix >> op and (op isa Brackets and ((op as Brackets).Type == BracketsType.Parenthesis or (op as Brackets).Type == BracketsType.Square) ))
			, (assignment >> e and (not (e isa Identifier and (e as Identifier).IsKeyword))  ) ^ Prefix(op, e, false)) | assignment
	
	assignment = ( or_expression >> l, (ASSIGN | ASSIGN_INPLACE) >> op, ((high_pr_pair >> r and (r isa Pair)) | assignment)  >> r, (--prefix_expression) >> tail ^ Infix(Identifier(tokenValue(op), false, false), l, getRight(r, tail))) \
					| or_expression

	//Example:
	//a = bar():
	//	pass
	//lock spam=foo(), eggs=bar():
	//    pass
	high_pr_pair = (~in_tuple, member_reference >> left, (begin_block_with_doc >> doc | begin_block), block >> right, DEDENT, prepend_eol ^ Pair(left, right, true, doc)) | member_reference

	def getRight(r, tail):
		return r if tail is null or len(tail) == 0
		
		result as Form
		for i in (tail as List).Reversed:
			if result is null:
				result = i
			else:
				result = Prefix(i, result, false)

		result = Prefix(r, result, false)
		return result
			

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


	high_priority_prefix = ( (STAR | (~~DOT, ~atom /*not float, ex: .001*/, DOT)) >> op, hp_tuple >> e ^ Prefix(Identifier(tokenValue(op), false, true), e, false)) | hp_tuple

	hp_tuple = (~in_tuple, ~in_lp_tuple, (prefix_of_brackets >> head, --(COMMA, prefix_of_brackets) >> tail, (COMMA | ("" and (len(tail)> 0))) ^ newTuple(head, tail)) ) | prefix_of_brackets
	
	prefix_of_brackets = (
							
							(prefix_of_brackets >> op and 
								(
									#Identifier + Brackets
									(op isa Identifier and not (op as Identifier).IsKeyword)
									#Brackets stick to brackets
									or (op isa Prefix and (op as Prefix).Operand isa Brackets)
									#Brackets stick to operator of
									or (op isa Infix and ((op as Infix).Operator as Identifier).Name == "of")
								)
							), 	sticky_brackets >> e ^ Prefix(op, e, false)
						 ) |\
						 (
						 	#Keyword + Brackets. Ex: def()
						 	#Keyword before brackets has lower priority then infix
						 	#Ex: for (n, a) in zip(names, attributes) =>  for ((n, a) in zip(names, attributes))
						 	#TODO: remove this. Rewrite closures evaluation.
							(prefix_of_brackets >> op and (op isa Identifier and (op as Identifier).IsKeyword)), ~(assignment >> io and (io isa Infix)), 
								sticky_brackets >> e ^ Prefix(op, e, false)
						 ) | of_operator

	infixr of_operator, OF, atom

	sticky_brackets = paren_brackets | square_brackets

	atom = exp_in_brackets | prefix_of_brackets | identifier | literal

	identifier = (ID >> s ^ Identifier(tokenValue(s), false, false)) | (KW >> s ^ Identifier(tokenValue(s), true, false))
	
	exp_in_brackets = paren_brackets | qq_brackets | square_brackets | curly_brackets

	paren_brackets = (LPAREN, enter_lp_tuple, ( form | "" ) >> f, RPAREN, leave_lp_tuple) ^  Brackets(f, BracketsType.Parenthesis)

	qq_brackets = ((QQ_BEGIN, INDENT, block >> f, DEDENT, QQ_END) | (QQ_BEGIN, form >> f, QQ_END)) ^ Brackets(f, BracketsType.QQ)
	
	square_brackets = (LBRACK
							,(
								(enter_lp_tuple, form >> f, RBRACK, leave_lp_tuple) |
								(form >> f, RBRACK)
								| RBRACK
							)
					) ^ Brackets(f, BracketsType.Square)
	
	curly_brackets = (LBRACE, ( form | "") >> f, RBRACE) ^ Brackets(f, BracketsType.Curly)
	
	literal = float | integer | string_literal 
	
	integer = (
		((MINUS | "") >> sign, NUM >> n and (IsValidLong(sign, n)), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.AllowLeadingSign, suffix)) \
		| ((MINUS | "") >> sign, (HEXNUM >> n and (IsValidHexLong(sign, n))), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.HexNumber, suffix))
	) >> i ^ Literal((i as IntegerLiteralExpression).Value, i)
	
	string_literal = (single_quote_string | double_quote_string) >> s ^ Literal(s, newStringLiteral(s))

	float = ( (fractional_constant >> n, (exponent_part | "") >> e , floating_suffix ) ^ newFloat(makeString(n,e))) | ((NUM >> n, exponent_part >> e, floating_suffix)  ^ newFloat(makeString(tokenValue(n),e)))

	fractional_constant = ((NUM >> a , DOT , NUM >> b) ^ makeString(tokenValue(a),".",tokenValue(b))) | ( (DOT , NUM >> b) ^ makeString(".",tokenValue(b)) ) | ( (NUM >> a , DOT, ~(ID)) ^ makeString(tokenValue(a), ".") )
    
	exponent_part = ( ("e" | "E") , exposignopt >> e , NUM >> d ) ^ makeString("e", e, tokenValue(d))

	exposignopt = ( (PLUS | MINUS) >> e ^ makeString(tokenValue(e)) ) | ""
	
	floating_suffix = "f" | "F" | ""

	prepend_eol = $(prependEol(input))
	
	def prependEol(input as OMetaInput):
		return success(input.Prepend(makeToken("eol"), input.Prev))

	enter_tuple = $(enterTuple(input))
	leave_tuple = $(leaveTuple(input))
	reset_tuple = $(resetTuple(input))

	in_tuple = ~~_ and inTuple(input) #tuple is started. It prevents hp_tuple and high_pr_pair to execute.
	
	def enterTuple(input as OMetaInput):
		return setTuple(input, getTuple(input) + 1)

	def leaveTuple(input as OMetaInput):
		return setTuple(input, getTuple(input) - 1) if inTuple(input)
		return success(input)
		
	def resetTuple(input as OMetaInput):
		return setTuple(input, 0)
		
	def inTuple(input as OMetaInput):
		return getTuple(input) > 0

	def getTuple(input as OMetaInput) as int:
		return input.GetMemo("tuple") or 0
		
	def setTuple(input as OMetaInput, value as int):
		return success(input.SetMemo("tuple", value))

	enter_lp_tuple = $(enterLPTuple(input))
	leave_lp_tuple = $(leaveLPTuple(input))
	reset_lp_tuple = $(resetLPTuple(input))

	in_lp_tuple = ~~_ and inLPTuple(input)
	
	def enterLPTuple(input as OMetaInput):
		return setLPTuple(input, getLPTuple(input) + 1)

	def leaveLPTuple(input as OMetaInput):
		return setLPTuple(input, getLPTuple(input) - 1) if inLPTuple(input)
		return success(input)
		
	def resetLPTuple(input as OMetaInput):
		return setLPTuple(input, 0)
		
	def inLPTuple(input as OMetaInput):
		return getLPTuple(input) > 0

	def getLPTuple(input as OMetaInput) as int:
		return input.GetMemo("lptuple") or 0
		
	def setLPTuple(input as OMetaInput, value as int):
		return success(input.SetMemo("lptuple", value))

def newFloat(t):
	value = double.Parse(t)
	return Literal(value, DoubleLiteralExpression(Value: value))

def newTuple(i):
	return Tuple(array(Form, i as List)) if i isa List 
	return Tuple((of Form:i))

def newTuple(head, tail as Form):
	return head if head isa Tuple and tail is null
	return Tuple((of Form:head,)) if tail is null
	return Tuple((head as Tuple).Forms + (tail as Tuple).Forms) if tail isa Tuple and head isa Tuple
	return Tuple((of Form:head,) + (tail as Tuple).Forms) if tail isa Tuple
	return Tuple(array(Form,[head,tail]))

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

	