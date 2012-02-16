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
		assign = "="

	stmt = stmt_block | stmt_line
	stmt_line = stmt_declaration | stmt_expression | stmt_macro
	
	stmt_expression = assignment
	stmt_block = stmt_for
	
	#expression = binary_expression | invocation | atom
	atom = reference | array_literal | boolean | literal
	
	literal = (Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si) | (Literal() >> l ^ (l as Literal).astLiteral)
	integer = Literal(Value: _ >> v and (v isa long)) >> l ^ (l as Literal).astLiteral
	
	string_interpolation = Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si
	
	booparser_string_interpolation = $(callExternalParser("string_interpolation_items", "Boo.OMeta.Parser.BooParser", input)) >> items ^ newStringInterpolation(items)
	
	array_literal = array_literal_multi
	
	array_literal_multi = Brackets(Kind: BracketType.Parenthesis, Form: Tuple(Forms: array_literal_multi_items >> tl)) ^ newArrayLiteral(null, tl)
	
	forArray[ruleName] = $(Apply(ruleName, (input.Head as Array)))
	
	array_literal_multi_items = (++assignment >> a, ~_) ^ a

	boolean = true_literal | false_literal
	
	true_literal = TRUE ^ [| true |]
	false_literal = FALSE ^ [| false |]
	
	#binary_operator = ( Identifier(Name: "or") ^ Token("or", "or")) | ( Identifier(Name: "and") ^ Token("and","and"))
	binary_operator = OR | AND | ASSIGN | IN
	
	binary_expression = Infix(Operator: binary_operator >> op, Left: assignment >> l, Right: assignment >> r) ^ newInfixExpression(op, l, r)
	
	reference = Identifier() >> r ^ ReferenceExpression(Name: (r as Identifier).Name)
	
	assignment = binary_expression | invocation | atom

	#declaration = ( ( Infix(Operator: AS, Left: Identifier(Name: _ >> name), Right: Identifier(Name: _ >> typeRef)) ) | Identifier(Name: _ >> name) ) ^ newDeclaration(Token(name, name), typeRef)
	
	stmt_declaration = (Infix(Operator: AS, Left: Identifier(Name: _ >> name), Right: type_reference >> typeRef) ^ newDeclaration(name, typeRef)) >> d ^ newDeclarationStatement(d, null)
	
	declaration = Identifier(Name: _ >> name) ^ newDeclaration(name, null)		
	
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
	
	empty_block = Block(Forms: (Identifier(Name: "pass"), ~_)) ^ AST.Block()

	
	
	stmt_for = Pair(Left:
						Prefix(Operator: FOR, Operand: Infix(Operator: IN, Left: declaration >> dl, Right: assignment >> e)),
					Right:
						block >> body) ^ newForStatement([dl], e, body, null, null)
						
	stmt_macro = (stmt_macro_head >> head | Pair(Left: stmt_macro_head >> head, Right: block >> b) ) ^ newMacro(makeToken((head as List)[0]), (head as List)[1], b, null)
	
	stmt_macro_head = Prefix(Operator: Identifier(Name: _ >> name), Operand: optional_assignment_list >> args ) ^ [name, args]
	
	optional_assignment_list = Tuple(Forms: (++assignment >> a, ~_)) ^ a | ~_
	
	type_reference = type_reference_simple | type_reference_array 
	
	type_reference_simple = Identifier(Name: _ >> name) ^ SimpleTypeReference(Name: name)
	
	type_reference_array = Brackets(Kind: BracketType.Parenthesis, Form: ranked_type_reference >> tr)  ^ tr
	
	ranked_type_reference = (type_reference >> type) | Tuple(Forms: (type_reference >> type, integer >> rank)) ^ ArrayTypeReference(ElementType: type, Rank: rank) 
	
	
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