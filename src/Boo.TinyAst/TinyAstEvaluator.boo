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
		DEF = "def"
		CLASS = "class"
		DOT = "."		
		PRIVATE = "private"
		PUBLIC = "public"
		INTERNAL = "internal"
		PROTECTED = "protected"
		FINAL = "final"
		STATIC = "static"
		VIRTUAL = "virtual"
		OVERRIDE = "override"
		TRANSIENT = "transient"
		ABSTRACT = "abstract"
		NEW = "new"
		EVENT = "event"
		GET = "get"
		SET = "set"
		RETURN = "return"

	stmt = type_member_stmt | stmt_block | stmt_line
	
	module_member = type_def | method
	type_member_stmt = (type_def | method) >> tm ^ TypeMemberStatement(TypeMember: tm)

	type_def = class_def | enum_def
	
	class_def = Pair(Left: Prefix(Operator: CLASS, Operand: id >> className),
						Right: class_body >> body) ^ newClass(null, null, className, null, null, body)
	
	class_body = Block(Forms: ( (empty_block ^ null) | (++class_member) >> b, nothing ) ) ^ b
	
	nothing = ~_
	
	class_member = type_def | method  | field | event_def | property_def
	
	enum_def = Pair(Left: Prefix(Operator: ENUM, Operand: id >> r), 
					Right: enum_body >> body
					) ^ newEnum(null, null, r, body)
	
	enum_body = (empty_block ^ null) | (Block(Forms: (++enum_field >> fields) ) ^ fields)
	
	enum_field = Infix(Operator:ASSIGN, Left: id >> name, Right: assignment >> e) | id >> name ^ newEnumField(null, name, e)
	
//	method = (
//		attributes >> attrs,
//		member_modifiers >> mod,
//		DEF, ID >> name,
//		optional_generic_parameters >> genericParameters,
//		method_parameters >> parameters,
//		attributes >> returnTypeAttributes, optional_type >> type,
//		block >> body
//	) ^ newGenericMethod(attrs, mod, name, genericParameters, parameters, returnTypeAttributes, type, body)	
	
	method = (--attributes_line >> att, here >> i, method_body >> body, inline_attributes >> in_att, member_modifiers >> mod, \
				prefix[DEF], prefix[id] >> name), next[i] ^ newGenericMethod([att, in_att], mod, name, null, [null, null], null, null, body)
	
	here = $(success(input, input))
	next[i] = $(success((i as OMetaInput).Tail, (i as OMetaInput).Tail))
	
	method_body = Pair(	Left: _ >> newInput, Right: block >> body), $(success(newInput, body))
	
//	field = (
//		attributes >> attrs,
//		member_modifiers >> mod,
//		ID >> name, optional_type >> type, field_initializer >> initializer
//	) ^ newField(attrs, mod, name, type, initializer)

	#field = --attributes_line >> att, inline_attributes >> in_att, member_modifiers >> mod ^ newField([att, in_att], mod, name, type, initializer)

	field = --attributes_line >> att, inline_attributes >> in_att, member_modifiers >> mod, field_initializer >> initializer \
				, optional_type >> type, id >> name ^ newField([att, in_att], mod, name, type, initializer)
				
	event_def = --attributes_line >> att, inline_attributes >> in_att, member_modifiers >> mod, optional_type >> type, prefix[EVENT], \
					optional_type >> type, id >> name ^ newEvent([att, in_att], mod, name, type)

//	property_def = (
//		attributes >> attrs,
//		member_modifiers >> mod,
//		ID >> name, property_parameters >> parameters, optional_type >> type,
//		begin_block,
//		(
//			(property_getter >> pg, property_setter >> ps)
//			| (property_setter >> ps, property_getter >> pg)
//			| (property_setter >> ps)
//			| (property_getter >> pg)
//		),
//		end_block
//	) ^ newProperty(attrs, mod, name, parameters, type, pg, ps)

	property_def = --attributes_line >> att, here >> i, property_body >> gs, inline_attributes >> in_att, member_modifiers >> mod, \
						(id |prefix[id])  >> name, next[i] ^ newProperty([att, in_att], mod, name, null, null, (gs as List)[0], (gs as List)[1]) /*TODO*/
						
	property_body = Pair(Left: _ >> newInput, Right: get_set >> gs), $(success(newInput, gs)) 
	
	get_set = Block(Forms: (
							(property_getter >> pg, property_setter >> ps, ~_)
							| (property_setter >> ps, property_getter >> pg, ~_)
							| (property_setter >> ps, ~_)
							| (property_getter >> pg, ~_)
							)
					) ^ [pg, ps]
	property_getter = accessor[GET]
	property_setter = accessor[SET]

//	accessor[key] = (
//		attributes >> attrs,
//		member_modifiers >> mod, 
//		(ID >> name and (tokenValue(name) == key)),
//		block >> body
//	) ^ newMethod(attrs, mod, tokenValue(name), [[],null], null, null, body)

	accessor[key] = --attributes_line >> att, Pair(Left: (inline_attributes >> in_att, member_modifiers >> mod, key >> name), Right: block >> body) \
						^ newMethod([att, in_att], mod, tokenValue(name), [[],null], null, null, body)

	prefix[rule] = Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))
	
	inline_attributes = inline_attributes_prescan | ("" ^ [])
							
	inline_attributes_prescan = (Prefix(Operator: attributes_group >> l, Operand: (inline_attributes_prescan >> r, _ >> newInput)), $(success(newInput, prepend(l, r))) )\
							| (Prefix(Operator: attributes_group >> l, Operand: (~inline_attributes_prescan, _ >> newInput)), $(success(newInput, l)) )

	def success(input, value):
		return SuccessfulMatch(input, value) if input isa OMetaInput
		return SuccessfulMatch(OMetaInput.Singleton(input), value)

	attributes_line =  Prefix(Operator: attributes_group >> l, Operand: attributes_line >> r) ^ prepend(l, r) | (attributes_group >> a ^ a)
	attributes_group = Brackets(Kind:BracketType.Square, Form: attribute_list >> attrs) ^ attrs
	
	attribute_list = Tuple(Forms: ++attribute >> a) ^ a | (attribute >> a ^ [a])
	
	attribute = (Prefix(Operator: qualified_name >> name, Operand: optional_invocation_arguments >> args) | qualified_name >> name) ^ newAttribute(name, args)
	
	member_modifiers = member_modifiers_prescan | ("" ^ [])
	
	member_modifiers_prescan = (Prefix(Operator: modifier >> l, Operand: (member_modifiers_prescan >> r, _ >> newInput)), $(success(newInput, prepend(l, r))) )\
						| (Prefix(Operator: modifier >> l, Operand: (~member_modifiers_prescan, _ >> newInput)), $(success(newInput, [l])) )
	
	modifier = (
				(PRIVATE ^ TypeMemberModifiers.Private) |
				(PUBLIC ^ TypeMemberModifiers.Public) |
				(INTERNAL ^ TypeMemberModifiers.Internal) |
				(PROTECTED ^ TypeMemberModifiers.Protected) | 
				(FINAL ^ TypeMemberModifiers.Final) | 
				(STATIC ^ TypeMemberModifiers.Static) | 
				(VIRTUAL ^ TypeMemberModifiers.Virtual) | 
				(OVERRIDE ^ TypeMemberModifiers.Override) | 
				(TRANSIENT ^ TypeMemberModifiers.Transient) | 
				(ABSTRACT ^ TypeMemberModifiers.Abstract) | 
				(NEW ^ TypeMemberModifiers.New)
			)
	
	field_initializer = (Infix(Operator: ASSIGN, Left: _ >> newInput, Right: assignment >> e), $(success(newInput, e)) )| ""
	
	optional_type = (Infix(Operator: AS, Left: _ >> newInput, Right: type_reference >> e), $(success(newInput, e)) )| ""
	
	id = Identifier(Name: _ >> name) ^ name
	
	qualified_name = (Infix(Operator: DOT, Left: id >> l, Right: qualified_name >> r) ^ ("$l.$r")) | id
	
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
	
	assignment = binary_expression | try_cast | prefix_expression | invocation | atom

	try_cast = Infix(Operator: AS, Left: assignment >> e, Right: type_reference >> typeRef)  ^ TryCastExpression(Target: e, Type: typeRef)

	stmt_declaration = (typed_declaration >> d
						| Infix(Operator: ASSIGN, Left: typed_declaration >> d, Right: assignment >> e)) ^ newDeclarationStatement(d, e)
	
	typed_declaration = Infix(Operator: AS, Left: Identifier(Name: _ >> name), Right: type_reference >> typeRef) ^ newDeclaration(name, typeRef)
	
	declaration = id >> name ^ newDeclaration(name, null)		
	
	prefix_expression = Prefix(Operator: prefix_operator >> op, Operand: assignment >> e) ^ newPrefixExpression(op, e)
	prefix_operator = NOT | MINUS
	
	invocation = invocation_expression
	invocation_expression = here >> i, member_reference >> mr, Prefix(Operator: reference >> target, Operand: invocation_arguments >> args) \
								, next[i] ^ newInvocation(getTarget(mr, target), args, null)
	
	def getTarget(l, r):
		return r if l is null
		return newMemberReference(l, (r as ReferenceExpression).Name)
	
	member_reference = (Infix(Operator: DOT, Left: member_reference_prescan >> e, Right: _ >> newInput), $(success(newInput, e))) | ""
	
	member_reference_prescan = Infix(Operator: DOT, Left: member_reference_prescan >> e, Right: id >> name) ^ newMemberReference(e, name) | reference
	
	invocation_arguments = Brackets(
								Kind: BracketType.Parenthesis,
								Form: (
										(Tuple(Forms: (++assignment >> a, ~_) ) ^ a) \
										| (assignment >> a ^ [a]) \
										| ((_ >> a and (a == null)) ^ [])
								) >> args
							) ^ args
	
	optional_invocation_arguments = invocation_arguments | (~~_ ^ null)
	block = empty_block | (Block(Forms: (++(stmt >> s ^ getStatement(s)) >> a, nothing) ) ^ newBlock(null, null, a, null)) | (stmt >> s ^ newBlock(null, null, s, null))
	
	empty_block = Block(Forms: (PASS, ~_)) ^ AST.Block()
	
	
	stmt_for = Pair(Left:
						Prefix(Operator: FOR, Operand: Infix(Operator: IN, Left: declaration >> dl, Right: assignment >> e)),
					Right:
						block >> body) ^ newForStatement([dl], e, body, null, null)
						
	stmt_if = Pair(Left:
						Prefix(Operator: IF, Operand: assignment >> e),
					Right: 
						block >> trueBlock) ^ newIfStatement(e, trueBlock, null)
	
	stmt_macro = (stmt_macro_head >> head | Pair(Left: stmt_macro_head >> head, Right: block >> b) ) ^ newMacro((head as List)[0], (head as List)[1], b, null)
	
	stmt_macro_head = Prefix(Operator: Identifier(Name: _ >> name), Operand: (optional_assignment_list >> args, ~_) ) ^ [name, args]
	
	optional_assignment_list = Tuple(Forms: (++assignment >> a, ~_)) ^ a | (assignment >> a ^ [a]) | ""
	
	type_reference = type_reference_simple | type_reference_array
	
	type_reference_simple = qualified_name >> name ^ SimpleTypeReference(Name: name)
	
	type_reference_array = Brackets(Kind: BracketType.Parenthesis, Form: ranked_type_reference >> tr)  ^ tr
	
	ranked_type_reference = (type_reference >> type) | Tuple(Forms: (type_reference >> type, integer >> rank)) ^ ArrayTypeReference(ElementType: type, Rank: rank)
	
	quasi_quote = quasi_quote_module | quasi_quote_member | quasi_quote_expression | quasi_quote_stmt
	
	quasi_quote_module = Brackets(Kind: BracketType.QQ, Form: Block(Forms: (module_member >> e, ~_))) ^ newQuasiquoteExpression(e)
	
	quasi_quote_member = Brackets(Kind: BracketType.QQ, Form: Block(Forms: (class_member >> e, ~_))) ^ newQuasiquoteExpression(e)
	
	quasi_quote_expression = Brackets(Kind: BracketType.QQ, Form: assignment >> e) ^ newQuasiquoteExpression(e)
	
	quasi_quote_stmt = Brackets(Kind: BracketType.QQ, Form: (qq_return | qq_macro) >> e) ^ newQuasiquoteExpression(e)
	
	qq_return = (RETURN | Prefix(Operator: RETURN, Operand: assignment >> e)) ^ ReturnStatement(Expression: e, Modifier: null)
	qq_macro = prefix[id] >> name, optional_assignment_list >> args ^ newMacro(name, args, null, null) 
//	qq_return = (RETURN, optional_assignment >> e, optional_stmt_modifier_node >> m) ^ ReturnStatement(Expression: e, Modifier: m)	
//	qq_macro = (ID >> name, assignment_list >> args, optional_stmt_modifier_node >> m) ^ newMacro(name, args, null, m)
	
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