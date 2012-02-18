namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.OMeta.Parser
import Boo.Lang.Compiler
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler.Ast as AST
import Boo.Lang.PatternMatching

macro keywordsAndTokens:
"""
Generates rules for tokens and keywords for TinyAst.

From:
	keywordsAndTokens:
		eq = "="
		OR = "or"
	
it generates:

	EQ = Identifier(Name: "=" >> name) ^ makeToken("EQ", name)
	OR = Identifier(Name: "or" >> name) ^ makeToken("OR", name)
"""
	block as AST.Block = keywordsAndTokens.ParentNode
	for stmt in keywordsAndTokens.Body.Statements:
		match stmt:
			case ExpressionStatement(Expression: [| $name = $pattern |]):
				e = [| $(ReferenceExpression(Name: name.ToString().ToUpper())) = Identifier(Name: $pattern >> name) ^ makeToken($(StringLiteralExpression(name.ToString().ToUpper())) , name) |]
				e.LexicalInfo = stmt.LexicalInfo
				block.Add(e)

ometa TinyAstEvaluator(compilerParameters as CompilerParameters):
	
	keywordsAndTokens:
		OR = "or"
		AND = "and"
		TRUE = "true"
		FALSE = "false"
		AS = "as"
		FOR = "for"
		IN = "in"
		NOT_IN = "not in"
		assign = "="
		OF = "of"
		IF = "if"
		NOT = "not"
		IS = "is"
		IS_NOT = "is not"
		plus = "+"
		minus = "-"
		star = "*"
		division = "/"
		assign_inplace = "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" | "<<=" | ">>="
		ENUM = "enum"
		PASS = "pass"

	stmt = stmt_block | stmt_line
	
	module_member = type_def

	type_def = enum_def
	
	enum_def = Pair(Left: Prefix(Operator:ENUM, Operand: id >> r), 
					Right: enum_body >> body
					) ^ newEnum(null, null, r, body)
	
	enum_body = (empty_block ^ null) | (Block(Forms: (++enum_field >> fields) ) ^ fields)
	
	enum_field = Infix(Operator:ASSIGN, Left: id >> name, Right: assignment >> e) | id >> name ^ newEnumField(null, name, e)
	
	id = Identifier(Name: _ >> name) ^ name
	
	stmt_line = stmt_declaration | stmt_expression | stmt_macro
	
	stmt_expression = assignment
	stmt_block = stmt_if | stmt_for
	
	atom = reference | array_literal | list_literal | boolean | literal | parenthesized_expression | quasi_quote
	
	literal = (Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si) | (Literal() >> l ^ (l as Literal).astLiteral)
	integer = Literal(Value: _ >> v and (v isa long)) >> l ^ (l as Literal).astLiteral
	
	string_interpolation = Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si
	
	booparser_string_interpolation = $(callExternalParser("string_interpolation_items", "Boo.OMeta.Parser.BooParser", input)) >> items ^ newStringInterpolation(items)
	
	array_literal = array_literal_multi
	
	array_literal_multi = Brackets(Kind: BracketType.Parenthesis, 
									Form: (
										Tuple(Forms: array_literal_multi_items >> tl) | 
										Pair(Left: array_literal_type >> type, Right: Tuple(Forms: array_literal_multi_items >> tl))
									)
							) ^ newArrayLiteral(type, tl)
	
	array_literal_type = Prefix(Operator: OF, Operand: type_reference >> type) ^ ArrayTypeReference(ElementType: type, Rank: null)
	
	array_literal_multi_items = (++assignment >> a, ~_) ^ a

	list_literal = Brackets(Kind: BracketType.Square,
								Form: (
									Tuple(Forms: ((++assignment >> a, ~_) ^ a) >> items)
								)
							) ^ newListLiteral(items)

	boolean = true_literal | false_literal
	
	true_literal = TRUE ^ [| true |]
	false_literal = FALSE ^ [| false |]
	
	parenthesized_expression = Brackets(Kind: BracketType.Parenthesis, Form: assignment >> e) ^ e
	
	binary_operator = OR | AND | ASSIGN_INPLACE | ASSIGN | IN | NOT_IN | IS | IS_NOT | PLUS | MINUS | STAR | DIVISION
	
	binary_expression = Infix(Operator: binary_operator >> op, Left: assignment >> l, Right: assignment >> r) ^ newInfixExpression(op, l, r)
	
	reference = id >> r ^ ReferenceExpression(Name: r)
	
	assignment = binary_expression | try_cast | prefix | invocation | atom

	try_cast = Infix(Operator: AS, Left: assignment >> e, Right: type_reference >> typeRef)  ^ TryCastExpression(Target: e, Type: typeRef)

	stmt_declaration = (typed_declaration >> d
						| Infix(Operator: ASSIGN, Left: typed_declaration >> d, Right: assignment >> e)) ^ newDeclarationStatement(d, e)
	
	typed_declaration = Infix(Operator: AS, Left: Identifier(Name: _ >> name), Right: type_reference >> typeRef) ^ newDeclaration(name, typeRef)
	
	declaration = id >> name ^ newDeclaration(name, null)		
	
	prefix = Prefix(Operator: prefix_operator >> op, Operand: assignment >> e) ^ newPrefixExpression(op, e)
	prefix_operator = NOT | MINUS
	
	invocation = invocation_expression
	invocation_expression = Prefix(Operator: 
										reference >> target, 
									Operand: 
										Brackets(Kind: BracketType.Parenthesis, 
												 Form: invocation_arguments >> args
										)
									) ^ newInvocation(target, args, null)
	
	invocation_arguments = (Tuple(Forms: (++assignment >> a, ~_) ) ^ a) | (assignment >> b ^ [b]) | ((_ >> a and (a == null)) ^ [])
	
	block = empty_block | (Block(Forms: (++(stmt >> s ^ getStatement(s)) >> a, ~_) ) ^ newBlock(null, null, a, null))
	
	empty_block = Block(Forms: (PASS, ~_)) ^ AST.Block()
	
	
	stmt_for = Pair(Left:
						Prefix(Operator: FOR, Operand: Infix(Operator: IN, Left: declaration >> dl, Right: assignment >> e)),
					Right:
						block >> body) ^ newForStatement([dl], e, body, null, null)
						
	stmt_if = Pair(Left:
						Prefix(Operator: IF, Operand: assignment >> e),
					Right: 
						block >> trueBlock) ^ newIfStatement(e, trueBlock, null)
	
	stmt_macro = (stmt_macro_head >> head | Pair(Left: stmt_macro_head >> head, Right: block >> b) ) ^ newMacro(makeToken((head as List)[0]), (head as List)[1], b, null)
	
	stmt_macro_head = Prefix(Operator: Identifier(Name: _ >> name), Operand: optional_assignment_list >> args ) ^ [name, args]
	
	optional_assignment_list = Tuple(Forms: (++assignment >> a, ~_)) ^ a | ~_
	
	type_reference = type_reference_simple | type_reference_array 
	
	type_reference_simple = Identifier(Name: _ >> name) ^ SimpleTypeReference(Name: name)
	
	type_reference_array = Brackets(Kind: BracketType.Parenthesis, Form: ranked_type_reference >> tr)  ^ tr
	
	ranked_type_reference = (type_reference >> type) | Tuple(Forms: (type_reference >> type, integer >> rank)) ^ ArrayTypeReference(ElementType: type, Rank: rank) 
	
	quasi_quote = Brackets(Kind: BracketType.QQ, Form: Block(Forms: (module_member >> e, ~_))) ^ newQuasiquoteExpression(e)
	
	
	def getStatement(s):
		return s if s isa Statement		
		return ExpressionStatement(s as Expression)	
						
	def callExternalParser(id, parser, input as OMetaInput):
		for r in compilerParameters.References:
			assemblyRef = r as Boo.Lang.Compiler.TypeSystem.Reflection.IAssemblyReference
			continue if assemblyRef is null
			
			assembly = assemblyRef.Assembly
			type = assembly.GetType(parser)
			break if type is not null
			
		return FailedMatch(input, RuleFailure("callExternalParser", PredicateFailure(parser))) if type is null
		
		#Save indent and wsa parameters (wsaLevel, indentStack, indentLevel)
		wsaLevel = input.GetMemo("wsaLevel")
		indentStack = input.GetMemo("indentStack")
		indentLevel = input.GetMemo("indentLevel")
		
		externalParser = Activator.CreateInstance(type)
		result = type.InvokeMember(id, BindingFlags.InvokeMethod, null as Binder, externalParser, (input,))
		
		//Restore indent and wsa parameters (wsaLevel, indentStack, indentLevel) after executing of external parser
		sm = result as SuccessfulMatch
		if sm is not null:
			sm.Input.SetMemo("wsaLevel", wsaLevel)
			sm.Input.SetMemo("indentStack", indentStack)
			sm.Input.SetMemo("indentLevel", indentLevel)
			return sm
			
		return result