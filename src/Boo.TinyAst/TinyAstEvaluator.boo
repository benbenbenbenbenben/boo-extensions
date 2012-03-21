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
				e = [| $(ReferenceExpression(Name: name.ToString().ToUpper())) = Identifier(Name: ($pattern >> name, ~_)) ^ makeToken($(StringLiteralExpression(name.ToString().ToUpper())) , name) |]
				e.LexicalInfo = stmt.LexicalInfo
				block.Add(e)

ometa TinyAstEvaluator(compilerParameters as CompilerParameters):
	
	keywordsAndTokens:
		OR = "or"
		AND = "and"
		TRUE = "true"
		FALSE = "false"
		AS = "as"
		CAST = "cast"
		FOR = "for"
		WHILE = "while"
		UNLESS = "unless"
		IN = "in"
		NOT_IN = "not in"
		OF = "of"
		IF = "if"
		NOT = "not"
		IS = "is"
		IS_NOT = "is not"
		increment = "++"
		decrement = "--"
		plus = "+"
		minus = "-"
		star = "*"
		division = "/"
		modulus = "%"
		semicolon = ";"
		bitwise_and = "&"
		bitwise_or = "|"		
		assign_inplace = "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" | "<<=" | ">>="
		bitwise_shift_left = "<<"
		bitwise_shift_right = ">>"
		equality = "=="
		inequality = "!="
		assign = "="		
		closure_separator = "=>"
		greater_than_eq = ">="
		greater_than = ">"
		less_than_eq = "<="
		less_than = "<"
		ENUM = "enum"
		PASS = "pass"
		DEF = "def"
		DO = "do"
		CLASS = "class"
		INTERFACE = "interface"
		CALLABLE = "callable"
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
		RAISE = "raise"
		THEN = "then"
		SPLICE_BEGIN = "$"

	expansion = module_member | stmt
	
	stmt = stmt_block | stmt_line
	
	module_member = assembly_attribute | type_def | method
	type_member_stmt = (type_def | method) >> tm ^ TypeMemberStatement(TypeMember: tm)

	type_def = class_def | enum_def | callable_def | interface_def
	
	class_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[CLASS], class_body >> body \
					, prefixOrId >> className, optional_super_types >> superTypes, next[i] ^ newClass([att, in_att], mod, className, null, superTypes, body)
	
	class_body = Pair(Left: _ >> newInput, Right: (empty_block | Block(Forms: ( ++class_member >> body, nothing) ) ^ body) ), $(success(newInput, body)) 

	interface_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[INTERFACE], interface_body >> body \
					, prefixOrId >> name, optional_super_types >> superTypes, next[i] ^ newInterface([att, in_att], mod, name, null, superTypes, body)

	
	optional_super_types = super_types | ""
	
	super_types = Brackets(Kind: BracketType.Parenthesis,
						Form: (
							(type_reference >> params)						
							| Tuple(
									Forms: (++type_reference >> params, ~_)
							)									
						)								
					) ^ (params if params isa List else [params])

	interface_body = Pair(Left: _ >> newInput, Right: (empty_block | Block(Forms: ( ++interface_member >> body, nothing) ) ^ body) ), $(success(newInput, body)) 
	
	interface_member = property_def | method_signature
	method_signature = "" #TODO

	nothing = ~_

	class_member = type_def | method | property_def | field | event_def  | enum_def
	
	enum_def = --attributes_line >> att, here >> i, prefix[ENUM], enum_body >> body, inline_attributes >> in_att, member_modifiers >> mod \
					, id >> name, next[i] ^ newEnum([att, in_att], mod, name, body)
	
	enum_body = Pair(
						Left: _ >> newInput, \
						Right: (
							(empty_block ^ null) \
							| (Block(Forms: (++enum_field >> fields) ) ^ fields)
						) >> body
					), $(success(newInput, body)) 
	
	enum_field = --attributes_line >> att, here >> i, inline_attributes >> in_att, \
					(Infix(Operator:ASSIGN, Left: id >> name, Right: assignment >> e) | id >> name), next[i] ^ newEnumField([att, in_att], name, e)
	
	callable_def = here >> i, member_modifiers >> mod, prefix[CALLABLE], optional_type >> type, prefix[id] >> name, \
					method_parameters >> parameters, next[i] ^ newCallable(mod, name, null, parameters, type)
	
	
	method = (--attributes_line >> att, inline_attributes >> in_att, member_modifiers >> mod, here >> i, prefix[DEF], method_body >> body, \
				 optional_type >> type, method_result_attributes >> ra, prefix[id] >> name, method_parameters >> parameters), next[i] ^ newGenericMethod([att, in_att], mod, name, null, parameters, ra, type, body)

	here = $(success(input, input))
	next[i] = $(success((i as OMetaInput).Tail, (i as OMetaInput).Tail))
	
	method_body = Pair(	Left: _ >> newInput, Right: (block >> body)), $(success(newInput, body))
	
	method_parameters = Brackets(Kind: BracketType.Parenthesis, Form: (method_parameter_list | ((_ >> p and (p is null)) ^ [[], null])) >> p) ^ p
	
	optional_parameters = method_parameters | ("" ^ [[], null])

	method_parameter_list = (parameter >> p ^ [[p], null]) \
							| (param_array >> pa ^ [null, pa]) \
							| (Tuple(Forms: (++parameter >> p, (param_array | "") >> pa, ~_)) ^ [p, pa])
			
	parameter = --attributes_line >> att, here >> i, inline_attributes >> in_att, optional_type >> type, id >> name, next[i] ^ newParameterDeclaration([att, in_att], name, type)
	param_array = --attributes_line >> att, inline_attributes >> in_att, optional_array_type >> type, prefix[STAR], id >> name ^ newParameterDeclaration([att, in_att], name, type)
	
	
	optional_array_type = (Infix(Operator: AS, Left: _ >> newInput, Right: type_reference_array >> e), $(success(newInput, e)) ) | ""
	
	method_result_attributes = (Prefix(Operator: _ >> newInput, Operand: inline_attributes >> attr and (len(attr) > 0)), $(success(newInput, attr))) | ""
	
	assembly_attribute = Brackets(Kind: BracketType.Square,
									Form: (
												(
													(assembly_attribute_first >> a ^ [a])
													| (	Tuple( Forms: (assembly_attribute_first >> a , ++attribute >> attr, ~_) ) ^ prepend(a, attr) ) 
												) >> attr
										
									) 
						) ^ attr
						
	assembly_attribute_first = Pair(Left: identifier["assembly"], Right: attribute >> a) ^ a
	
	event_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, optional_type >> type, prefix[EVENT], \
					optional_type >> type, id >> name, next[i] ^ newEvent([att, in_att], mod, name, type)


	property_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, property_body >> gs, \
						optional_type >> type, (id |prefix[id])  >> name, property_parameters >> params, next[i] ^ newProperty([att, in_att], mod, name, params, type, (gs as List)[0], (gs as List)[1]) /*TODO*/
						
	property_body = Pair(Left: _ >> newInput, Right: get_set >> gs), $(success(newInput, gs)) 
	
	property_parameters = Brackets(
									Kind: BracketType.Square, 
									Form: (
											Tuple(Forms: ((++parameter >> p, ~_) ^ p) )
											| (parameter >> p ^ [p]) 
											| ( (_ >> p and (p is null)) ^ [null])
									) >> p
							) ^ p | ""
	
	get_set = Block(Forms: (
							(property_getter >> pg, property_setter >> ps, ~_)
							| (property_setter >> ps, property_getter >> pg, ~_)
							| (property_setter >> ps, ~_)
							| (property_getter >> pg, ~_)
							)
					) ^ [pg, ps]
					
	property_getter = accessor[GET]
	property_setter = accessor[SET]


	accessor[key] = --attributes_line >> att, Pair(Left: (inline_attributes >> in_att, member_modifiers >> mod, key >> name), Right: (block) >> body) \
						^ newMethod([att, in_att], mod, tokenValue(name), [[],null], null, null, body)

	prefix[rule] = Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))
	optional_prefix[rule] = (Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))) | ""
	
	optional[rule] = (rule >> e, $(success(e, input))) | ""
	
	prefix2[rule] = Prefix(Operand: rule >> e, Operator: _ >> newInput), $(success(newInput, e))
	optional_prefix2[rule] = (Prefix(Operand: rule >> e, Operator: _ >> newInput), $(success(newInput, e))) | ""
	
	prefixOrId = id \
					|(
							Prefix(Operator: id >> e, Operand: _ >> newInput), $(success(newInput, e))
					)
	prefixOrInfix = (Prefix(Operator: _ >> e, Operand: _ >> newInput), $(success(OMetaInput.For([e, newInput]), e))) \
					| (Infix() >> e, $(success(e, e)))
	
	inline_attributes = inline_attributes_prescan | ("" ^ [])
							
	inline_attributes_prescan = (Prefix(Operator: attributes_group >> l, Operand: (inline_attributes_prescan >> r, _ >> newInput)), $(success(newInput, prepend(l, r))) )\
							| (Prefix(Operator: attributes_group >> l, Operand: (~inline_attributes_prescan, _ >> newInput)), $(success(newInput, l)) )\
							| attributes_group



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
	field = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod\
				, field_initializer >> initializer \
				, optional_type >> type and (type is not null or initializer is not null) \
				, id >> name, next[i] ^ newField([att, in_att], mod, name, type, initializer)
				
//	def getInitializer(initializer, body):
//		return newBlockExpression(null, null, initializer, body) if body is not null and initializer is not null
//		return initializer

	field_initializer = (Infix(Operator: ASSIGN, Left: _ >> newInput, Right: (assignment | block_expression) >> e), $(success(newInput, e)) )| ""
	
	optional_type = (Infix(Operator: AS, Left: _ >> newInput, Right: type_reference >> e), $(success(newInput, e)) )| ""
	
	id = Identifier(Name: _ >> name, IsKeyword: _ >> k and (k == false), IsSymbol: _ >> s and (s == false)) ^ name
	
	identifier[n] = Identifier(Name: _ >> name and (name == n)) ^ name
	
	qualified_name = (Infix(Operator: DOT, Left: qualified_name >> l, Right: id >> r) ^ ("$l.$r")) | id
	
	stmt_line = stmt_declaration \
				| stmt_expression \
				| stmt_return \
				| stmt_macro \
				| stmt_raise \
				| stmt_unpack
	
	stmt_expression = assignment >> a ^ ExpressionStatement(a as Expression) | stmt_expression_block | ((block_expression >> e) ^ ExpressionStatement(Expression: e))
	
	//stmt_expression_block = invocation_with_block_assignment | closure_block_assignment | dsl_friendly_invocation_assignment
	stmt_expression_block = Infix(
												Operator: (ASSIGN | ASSIGN_INPLACE) >> op,
												Left: expression >> l,
												Right: block_expression >> r
										) ^ ExpressionStatement(newInfixExpression(op, l, r))
	
	block_expression = invocation_with_block | closure_block | dsl_friendly_invocation
	
	closure_block = here >> i, multiline_pair_block >> body, \
										 closure_block_left >> parameters, next[i] ^ newBlockExpression(null, null, parameters, body)
	
	invocation_with_block = here >> i, \
										prefix[invocation] >> e, multiline_pair_block >> body, closure_block_left >> parameters \
										, next[i] ^ newInvocationWithBlock(e, newBlockExpression(null, null, parameters, body))
	
	dsl_friendly_invocation = ~_

	multiline_pair_block = Pair(IsMultiline: _ >> ml and (ml == true), Right: block >> body, Left: _ >> newInput), $(success(newInput, body))

	block_expression_left = closure_block_left
		
	closure_block_left = ((prefix[DEF] | prefix[DO]), method_parameters >> parameters) ^ parameters \
						| ( (DEF | DO) ^ [[], null])
						

	
	stmt_block = stmt_if | stmt_for | stmt_while
	
	stmt_unpack = here >> i, prefixOrInfix, Infix(Operator: ASSIGN, Left: declaration_list >> declarations, Right: rvalue >> e), optional[stmt_modifier] >> m, next[i] ^ newUnpackStatement(declarations, e, m)
	
	atom = reference | array_literal | list_literal | boolean | literal | parenthesized_expression | quasi_quote | splice_expression | closure
	
	literal = (Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si) | (Literal() >> l ^ (l as Literal).astLiteral)
	integer = Literal(Value: _ >> v and (v isa long)) >> l ^ (l as Literal).astLiteral
	
	splice_expression = prefix[SPLICE_BEGIN], atom >> e ^ SpliceExpression(Expression: e)
	
	string_interpolation = Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si
	
	booparser_string_interpolation = $(callExternalParser("string_interpolation_items", "Boo.OMeta.Parser.BooParser", input)) >> items ^ newStringInterpolation(items)
	
	closure	= Brackets( 
						Kind: BracketType.Curly,
						Form: (
								Infix(Operator: CLOSURE_SEPARATOR, Left: parameter >> p, Right: closure_stmt_list >> e) \
								| (closure_stmt_list >> e)
						)
				) ^ newBlockExpression(null, null, [([]if p is null else [p]),null], newBlock(null, null, e, null))
	
	closure_stmt_list = (closure_stmt >> s ^ [s]) | (Block(Forms: ++closure_stmt >> s) ^ s)
	
	closure_stmt = closure_stmt_expression | stmt_return | stmt_raise | stmt_unpack
	
	closure_stmt_expression = here >> i, (assignment >> e | (prefix[assignment] >> e, stmt_modifier >> m) ), next[i] ^ ExpressionStatement(Expression: e, Modifier: m)
		
	array_literal = array_literal_multi | array_literal_multi_typed
	
	array_literal_multi = Brackets(
									Kind: BracketType.Parenthesis, 
									Form: ( 
										Tuple(Forms: (++assignment >> items, ~_) )
									)
								) ^ newArrayLiteral(null, items)
	array_literal_multi_typed = Brackets(
									Kind: BracketType.Parenthesis, 
									Form: ( prefix[OF], Pair(Left: type_reference >> type, Right: Tuple(Forms: (++assignment >> items, ~_) ))
									)
								) ^ newArrayLiteral(ArrayTypeReference(ElementType: type, Rank: null), items)
	
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
	
	binary_operator = OR | AND | ASSIGN_INPLACE | ASSIGN | IN | NOT_IN | IS | IS_NOT | PLUS | MINUS | STAR \
					| DIVISION | BITWISE_SHIFT_LEFT | BITWISE_SHIFT_RIGHT | GREATER_THAN_EQ | GREATER_THAN \
					| LESS_THAN_EQ | LESS_THAN | EQUALITY | INEQUALITY | MODULUS

	
	binary_expression = Infix(Operator: binary_operator >> op, Left: assignment >> l, Right: (assignment >> r)) ^ newInfixExpression(op, l, r)
	
	reference = id >> r ^ ReferenceExpression(Name: r)
	
	assignment = binary_expression | try_cast | cast_operator | prefix_expression | invocation | atom | member_reference | expression
	
	expression = generator_expression | (~Infix(Operator: (ASSIGN | ASSIGN_INPLACE)), assignment)
	
	generator_expression = here >> i, prefix[assignment] >> projection, ++generator_expression_body >> body, nothing, next[i] ^ newGeneratorExpression(projection, body)	
	
	generator_expression_body = here >> i, prefix[FOR], optional_prefix2[filter]>> f \
								, Infix(
									Operator: IN,
									Left: declaration_list >> dl,
									Right: rvalue >> r												
								), next[i] ^ newGeneratorExpressionBody(dl, r, f)
	
	filter = "" #TODO
	
	declaration_list = Tuple(Forms: (--declaration >> l)) | ((declaration >> l ^ [l]) >> l) ^ l

	try_cast = Infix(Operator: AS, Left: assignment >> e, Right: type_reference >> typeRef)  ^ TryCastExpression(Target: e, Type: typeRef)
	
	cast_operator = Infix(Operator: CAST, Left: assignment >> e, Right: type_reference >> typeRef)  ^ CastExpression(Target: e, Type: typeRef)

	stmt_declaration = (typed_declaration >> d
						| Infix(Operator: ASSIGN, Left: typed_declaration >> d, Right: assignment >> e)) ^ newDeclarationStatement(d, e)
	
	typed_declaration = Infix(Operator: AS, Left: Identifier(Name: _ >> name), Right: type_reference >> typeRef) ^ newDeclaration(name, typeRef)
	
	declaration = optional_type >> typeRef, id >> name ^ newDeclaration(name, typeRef)		
	

	prefix_expression = Prefix(Operator: prefix_operator >> op, Operand: assignment >> e) ^ newPrefixExpression(op, e)
	prefix_operator = NOT | MINUS | INCREMENT | DECREMENT

	
	invocation = invocation_expression
	invocation_expression = here >> i, member_reference_left >> mr, Prefix(Operator: (reference | invocation | atom) >> target, Operand: invocation_arguments >> args) \
								, next[i] ^ newInvocation(getTarget(mr, target), args, null)
	
	def getTarget(l, r):
		return r if l is null
		return newMemberReference(l, (r as ReferenceExpression).Name)
	
	member_reference_left = (Infix(Operator: DOT, Left: member_reference >> e, Right: _ >> newInput), $(success(newInput, e))) | ""
	
	member_reference = Infix(Operator: DOT, Left: member_reference >> e, Right: id >> name) ^ newMemberReference(e, name) | (reference | invocation | invocation_expression | atom)
	
	invocation_arguments = Brackets(
								Kind: BracketType.Parenthesis,
								Form: (
										(Tuple(Forms: (++(invocation_argument) >> a, ~_) ) ^ a) \
										| (invocation_argument >> a ^ [a]) \
										| ((_ >> a and (a == null)) ^ [])
								) >> args
							) ^ args

	
	invocation_argument = named_argument | assignment
	
	named_argument = Pair(IsMultiline: _ >> ml and (ml == false), Left: id >> name, Right: assignment >> value) ^ newNamedArgument(name, value)
	
	optional_invocation_arguments = invocation_arguments | (~~_ ^ null)
	block = empty_block | (Block(Forms: (++(stmt >> s ^ getStatement(s)) >> a, nothing) ) ^ newBlock(null, null, a, null)) | (stmt >> s ^ newBlock(null, null, s, null))
	
	empty_block = Block(Forms: (PASS, ~_)) ^ AST.Block()
	
	stmt_for = here >> i, prefix[FOR], Pair(Left:
												Infix(
													Operator: IN,
													Left: declaration_list >> dl,
													Right: rvalue >> r												
												),
											Right:
												block >> body), next[i] ^ newForStatement(dl, r, body, null, null)
												
	rvalue = assignment_list >> items ^ newRValue(items)
						
	stmt_if = here >> i, prefix[IF], Pair(Left:
						assignment >> e,
					Right: 
						block >> trueBlock), next[i] ^ newIfStatement(e, trueBlock, null)
				
	stmt_while = here >> i, prefix[WHILE], Pair(Left: (assignment >> e), Right: block >> body), or_block >> orBlock, then_block >> thenBlock, next[i] ^ newWhileStatement(e, body, orBlock, thenBlock)
	
	or_block = Pair(Left: OR, Right: block >> orBlock) | "" ^ orBlock
	then_block = Pair(Left: THEN, Right: block >> thenBlock) | "" ^ thenBlock
	
	stmt_macro = macro_id >> name |\
					Pair(Left: macro_id >> name, Right: block >> b) | \
					Prefix(
						Operator: macro_id >> name, 
						Operand: (optional_assignment_list >> args, ~_) | Pair(Left: optional_assignment_list >> args, Right: block >> b)					
					) ^ newMacro(name, args, b, null)
					
	macro_id = Identifier(Name: _ >> name, IsKeyword: _ >> k and (k == false)) ^ name
	
	stmt_return = here >> i, (RETURN | prefix[RETURN], (assignment >> e | (prefix[assignment] >> e, stmt_modifier >> m) | block_expression >> e ) ), next[i] ^ ReturnStatement(Expression: e, Modifier: m) 

	stmt_raise = here >> i, prefix[RAISE], (expression >> e | (prefix[expression] >> e, stmt_modifier >> m)), next[i] ^ RaiseStatement(Exception: e, Modifier: m)
	
	stmt_modifier = prefix[stmt_modifier_type] >> t, assignment >> e ^ newStatementModifier(t, e)
	
	stmt_modifier_type = (IF ^ StatementModifierType.If) | (UNLESS ^ StatementModifierType.Unless)
	
	assignment_list = Tuple(Forms: (++assignment >> a, ~_)) ^ a | (assignment >> a ^ [a])
	optional_assignment_list = assignment_list | ""
	
	type_reference = type_reference_simple | type_reference_array | type_reference_splice | type_reference_callable
	
	type_reference_simple = qualified_name >> name ^ SimpleTypeReference(Name: name)
	
	type_reference_splice = prefix[SPLICE_BEGIN], atom >> e ^ SpliceTypeReference(Expression: e)
	
	type_reference_array = Brackets(Kind: BracketType.Parenthesis, Form: ranked_type_reference >> tr)  ^ tr
	
	ranked_type_reference = (type_reference >> type) | Tuple(Forms: (type_reference >> type, integer >> rank)) ^ ArrayTypeReference(ElementType: type, Rank: rank)
	
	type_reference_callable = optional_type >> type, prefix[CALLABLE], \
								Brackets(Kind: BracketType.Parenthesis,
									Form: (
										(type_reference >> params)
										| (param_array_reference >> paramArray)
										| Tuple(
												Forms: (++type_reference >> params, (param_array_reference|"") >> paramArray, ~_)
											)									
									)								
								) ^ newCallableTypeReference((params if (params isa List) else [params]), paramArray, type)


	param_array_reference = here >> i, prefix[STAR], type_reference >> type, next[i] ^ newParameterDeclaration(null, "arg0", type)

	quasi_quote = quasi_quote_member | quasi_quote_module | quasi_quote_expression | quasi_quote_stmt
	
	quasi_quote_module = Brackets(Kind: BracketType.QQ, Form: Block(Forms: (--module_member >> members, --stmt >> stmts, ~_))) ^ newQuasiquoteExpression(newModule(null, null, [], members, stmts))
	
	quasi_quote_member = Brackets(Kind: BracketType.QQ, Form: Block(Forms: (class_member >> e, ~_))) ^ newQuasiquoteExpression(e)
	
	quasi_quote_expression = Brackets(Kind: BracketType.QQ, Form: assignment >> e) ^ newQuasiquoteExpression(e)
	
	quasi_quote_stmt = Brackets(Kind: BracketType.QQ, Form: (qq_return | qq_macro) >> e) ^ newQuasiquoteExpression(e)
	
	qq_return = (RETURN | Prefix(Operator: RETURN, Operand: assignment >> e)) ^ ReturnStatement(Expression: e, Modifier: null)
	qq_macro = prefix[id] >> name, optional_assignment_list >> args ^ newMacro(name, args, null, null) 
	
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

def success(input, value):
	return SuccessfulMatch(input, value) if input isa OMetaInput
	return SuccessfulMatch(OMetaInput.Singleton(input), value)

def success(input as OMetaInput):
	return success(input, null)
	
def fail(input as OMetaInput, reason as RuleFailure):
	return FailedMatch(input, reason)