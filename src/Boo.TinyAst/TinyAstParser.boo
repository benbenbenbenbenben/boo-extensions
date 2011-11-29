namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.Adt
import Boo.OMeta.Parser
import Boo.Lang.Compiler.Ast
import System.Globalization

data Form = \
	Identifier(Name as string) | \
	Quote(Form as Form, Quote as string)  |\
	Literal(Value as object) |\
	Infix(Operator as string, Left as Form, Right as Form) |\
	Prefix(Operator as Form, Operand as Form) |\
	Tuple(Forms as (Form))

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

	keywords "and", "as", "import", "from", "namespace", "or"
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
		(--form_stmt ) >> forms,
		--EOL
	) ^ newModule(ns, s, ids, [], forms)

	//Parsing of forms	
	form_stmt = form >> f, EOL ^ newMacroStatement(f)
	form = tuple | infix_operator | identifier | literal
	
	literal = integer
	integer = (
		((MINUS | "") >> sign, NUM >> n and (IsValidLong(sign, n)), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.AllowLeadingSign, suffix)) \
		| ((MINUS | "") >> sign, (HEXNUM >> n and (IsValidHexLong(sign, n))), ("L" | "l" | "") >> suffix ^ newInteger(sign, n, NumberStyles.HexNumber, suffix))
	) >> i ^ Literal((i as IntegerLiteralExpression).Value)

	macro_prefix = ""

	infix_operator = assignment
	infixr assignment, (ASSIGN | ASSIGN_INPLACE), or_expression
	infix or_expression, OR, and_expression
	infix and_expression, AND, form
	
	identifier = ID >> s ^ Identifier(tokenValue(s))
	tuple = LPAREN, form_list >> f, optional_comma, RPAREN ^ newTuple(f) 
	optional_comma = COMMA | ""
	list_of form
	list_of literal
	
	def newTuple(f as List):
		return Tuple(array(Form,f))

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
		

	