namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.Adt
import Boo.OMeta.Parser
import Boo.Lang.Compiler.Ast
import System.Globalization

data Form = \
	Identifier(Name as string) | \
	Quote(Form)  |\
	Literal(Value as object) |\
	Infix(Operator as string, Left, Right) |\
	Prefix(Operator, Operand) |\
	Tuple(Forms as (Form)) |\
	Pair(Left, Right) |\
	Block(Forms as (Form))
	

macro infix:
	l, op, r = infix.Arguments
	return ExpressionStatement([| $l = ((($l >> l, $op >> op, $r >> r) ^ Infix(tokenValue(op), l, r)) | $r) |])
	
macro infixr:
	l, op, r = infixr.Arguments
	return ExpressionStatement([| $l = ((($r >> l, $op >> op, $l >> r) ^ Infix(tokenValue(op), l, r)) | $r) |])
	
macro prefix:
	rule, op, next = prefix.Arguments
	return ExpressionStatement([| $rule = ($op >> op, $rule >> e) ^ Prefix(op, e) | $next |])

ometa TinyAstParser < WhitespaceSensitiveTokenizer:
	tokens:
		assign = "="
		assign_inplace = "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" | "<<=" | ">>="
		
		plus = "+"
		minus = "-"
		colon = ":"
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

	keywords "and", "as", "from", "import", "in", "namespace", "not", "or"
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
	
	line_continuation = "\\", newline	
	multi_line_comment = "/*", --(~"*/", (multi_line_comment | _)), "*/"
	line_comment = ('#' | "//"), --(~newline, _)	
	
	namespace_declaration = (NAMESPACE, qualified_name)
	import_declaration = ( (IMPORT, qualified_name >> qn), (((FROM, (dqs | sqs | qualified_name)) | "") >> assembly), ( (AS, ID) | "") >> alias, eol) ^ newImport(qn, assembly, alias)

	module = (
		--EOL,
		((tqs >> s , EOL) | ""),
		--EOL,
		((namespace_declaration >> ns , EOL) | ""),
		--import_declaration >> ids,
		(block >> b ^ [newMacroStatement(b)]) >> forms,
		--EOL
	) ^ newModule(ns, s, ids, [], forms)

	//Parsing of forms

	form = single_line_pair | (prefix_operator >> p and (p isa Prefix)) | (infix_operator >> i and (i isa Infix)) | atom

	form_stmt = ((multi_line_pair >> f) | (form >> f, eol)) ^ f

	block = (++(form_stmt) >> forms) ^ Block(array(Form,forms as List))		

	single_line_pair = (single_line_pair_prescan >> p and (p isa Pair)) ^ p
	single_line_pair_prescan = (single_line_pair_prescan >> left, COLON, form >> right ^ Pair(left, right)) | prefix_operator | infix_operator | atom

	multi_line_pair = (multi_line_pair_prescan >> p and (p isa Pair)) ^ p
	multi_line_pair_prescan = (multi_line_pair_prescan >> left, begin_block, block >> right, end_block ^ Pair(left, right)) | prefix_operator | infix_operator | atom

	begin_block = COLON, INDENT
	end_block = DEDENT
	
	literal = integer | string_literal
	integer = (
		((MINUS | "") >> sign, NUM >> n and (IsValidLong(sign, n)), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.AllowLeadingSign, suffix)) \
		| ((MINUS | "") >> sign, (HEXNUM >> n and (IsValidHexLong(sign, n))), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.HexNumber, suffix))
	) >> i ^ Literal((i as IntegerLiteralExpression).Value)

	string_literal = (sqs | dqs) >> s ^ Literal(s)
	
	infix_operator = assignment 
	infixr assignment, (ASSIGN | ASSIGN_INPLACE), or_expression
	infix or_expression, OR, and_expression
	infix and_expression, AND, membership_expression
	infix membership_expression, (IN | ((NOT, IN) ^ makeToken("not in"))), as_operator	
	infix as_operator, AS, atom
	
	atom = prefix_operator | tuple | identifier | literal

	identifier = (ID | KW) >> s ^ Identifier(tokenValue(s))
	tuple = tuple1 | tuple2
	tuple1 = (LPAREN, (tuple_item_list | (tuple_item >> f ^ [f]) | ("" ^ [])) >> f, optional_comma, RPAREN) ^ newTuple(f)
	tuple_item = tuple1 | identifier | literal	
	tuple2 = (tuple_item_list >> f) ^ newTuple(f)
	
	tuple_item_list = (((tuple_item >> first), ++((COMMA, tuple_item >> e) ^ e) >> rest) ^ prepend(first, rest))
	
	list_of form
	optional_comma = COMMA | ""

	prefix_operator = identifier >> f1, form >> f2 ^ Prefix(f1, f2)

	def newTuple(f):
		return Tuple(array(Form,f as List))

	def newMacroStatement(data):
		m = MacroStatement("tinyAst")
		m.Annotate("tinyAst", data)
		return m
	
ometa TinyAstEvaluator:
	ast = stmt	
	stmt = stmt_line
	stmt_line = stmt_expression
	stmt_expression = assignment
	
	expression = binary_expression | atom 	
	atom = boolean

	boolean = true_literal | false_literal
	
	true_literal = Identifier(Name:"true") ^ [| true |]
	
	false_literal = Identifier(Name: "false") ^ [| false |]	
	
	binary_operator = ( (_ >> a and (a == "or" )) ^ Token("or", "or")) | ( (_ >> a and (a == "and" )) ^ Token("and","and"))		
	
	binary_expression = Infix(Operator:binary_operator >> op, Left:expression >> l, Right:expression >> r) ^ newInfixExpression(op, l, r)
	
	reference = Identifier() >> r ^ ReferenceExpression(Name: (r as Identifier).Name)
	
	assignment = Infix(Operator:"=", Left:reference >> l, Right: expression >> r) ^ newInfixExpression(Token("=", "="), l, r)
		

	