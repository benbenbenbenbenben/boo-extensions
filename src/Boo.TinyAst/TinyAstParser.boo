namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.Adt
import Boo.OMeta.Parser
import Boo.Lang.Compiler.Ast

data Form = \
	Identifier(name as string) | \
	Quote(form as Form, quote as string)  |\
	Literal(value as object) | \
	Infix(operator as string, left as Form, right as Form) |\
	Prefix(operator as Form, operand as Form)

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

		dot = "."
		kw = (keywords >> value, ~(letter | digit | '_')) ^ value
		tdq = '"""'
		dq = '"'
		sq = "'"
		id = ((letter | '_') >> p, --(letter | digit | '_') >> s) ^ makeString(p, s)

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
		(--(form >> f ^ newMacroStatement(f) ) ) >> forms,
		--EOL
	) ^ newModule(ns, s, ids, [], forms)

	//Parsing of forms
	form = (infix_operator | identifier) >> f, eol ^ f

	macro_prefix = ""
	
	infix_operator = assignment	
	infixr assignment, (ASSIGN | ASSIGN_INPLACE), or_expression
	infix or_expression, OR, and_expression
	infix and_expression, AND, atom
	
	atom = identifier
	
	identifier = ID >> s ^ Identifier(tokenValue(s))
	

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
	
	true_literal = Identifier(name:"true") ^ [| true |]
	
	false_literal = Identifier(name: "false") ^ [| false |]	
	
	binary_operator = ( (_ >> a and (a == "or" )) ^ Token("or", "or")) | ( (_ >> a and (a == "and" )) ^ Token("and","and"))		
	
	binary_expression = Infix(operator:binary_operator >> op, left:expression >> l, right:expression >> r) ^ newInfixExpression(op, l, r)
	
	reference = Identifier() >> r ^ ReferenceExpression(Name: (r as Identifier).name)
	
	assignment = Infix(operator:"=", left:reference >> l, right: expression >> r) ^ newInfixExpression(Token("=", "="), l, r)
		

	