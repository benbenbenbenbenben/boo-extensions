﻿namespace Boo.TinyAst

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
		ELSE = "else"
		ELIF = "elif"
		NOT = "not"
		ISA = "isa"
		IS = "is"
		IS_NOT = "is not"
		increment = "++"
		decrement = "--"
		plus = "+"
		minus = "-"
		ones_complement = "~"
		star = "*"
		division = "/"
		modulus = "%"
		semicolon = ";"
		bitwise_and = "&"
		bitwise_or = "|"
		xor = "^"
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
		STRUCT = "struct"
		CONSTRUCTOR = "constructor"
		TYPEOF = "typeof"
		TRY = "try"
		EXCEPT = "except"
		FAILURE = "failure"
		ENSURE = "ensure"
		YIELD = "yield"

	expansion = module_member | stmt
	
	stmt = stmt_block | stmt_line
	
	module_member = assembly_attribute | type_def | method
	type_member_stmt = (type_def | method) >> tm ^ TypeMemberStatement(TypeMember: tm)

	type_def = class_def | struct_def | interface_def | enum_def | callable_def
	
	class_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[CLASS], class_body >> body \
					, optional_prefix_operand[super_types] >> superTypes, prefix_or_rule[id] >> className, optional[generic_parameters] >> gp, nothing \
					, next[i] ^ newClass([att, in_att], mod, className, gp, superTypes, body)

	struct_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[STRUCT], struct_body >> body \
					, optional_prefix_operand[super_types] >> superTypes, prefix_or_rule[id] >> structName, optional[generic_parameters] >> gp, nothing \
					, next[i] ^ newStruct([att, in_att], mod, structName, gp, superTypes, body)

	class_body = Pair(Left: _ >> newInput, Right: (empty_block | Block(Forms: ( ++class_member >> body, nothing) ) ^ body) ), $(success(newInput, body)) 
	struct_body = Pair(Left: _ >> newInput, Right: (empty_block | Block(Forms: ( ++struct_member >> body, nothing) ) ^ body) ), $(success(newInput, body)) 

	interface_def = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[INTERFACE], interface_body >> body \
					, optional_prefix_operand[super_types] >> superTypes, prefix_or_rule[id] >> name, optional[generic_parameters] >> gp, nothing \
					, next[i] ^ newInterface([att, in_att], mod, name, gp, superTypes, body)

	super_types = Brackets(Type: BracketsType.Parenthesis,
						Form: (
							(type_reference >> params)						
							| Tuple(
									Forms: (++type_reference >> params, ~_)
							)
							| ((_ >> params and (params is null)) ^ []) >> params
						)								
					) ^ (params if params isa List else [params])

	interface_body = Pair(Left: _ >> newInput, Right: (empty_block | Block(Forms: ( ++interface_member >> body, nothing) ) ^ body) ), $(success(newInput, body)) 
	
	interface_member = property_def | method_signature
	
	method_signature = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[DEF] \
				 , optional_type >> type, method_result_attributes >> ra, prefix_operand[method_parameters] >> parameters \
				 , prefix_or_rule[id] >> name, optional[generic_parameters] >> gp, nothing, next[i] \
				 ^ newGenericMethod([att, in_att], mod, name, gp, parameters, ra, type, null)

	nothing = ~_

	class_member = type_def | method | property_def | field | event_def  | enum_def
	
	struct_member = constructor_method | method | field 
	
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
	
	callable_def = here >> i, member_modifiers >> mod, prefix[CALLABLE], optional_type >> type, prefix_operand[method_parameters] >> parameters \
				 , prefix_or_rule[id] >> name, optional[generic_parameters] >> gp, nothing, next[i] ^ newCallable(mod, name, gp, parameters, type)
	
	
	method = --attributes_line >> att, here >> i, inline_attributes >> in_att, member_modifiers >> mod, prefix[DEF], method_body >> body \
				 , optional_type >> type, method_result_attributes >> ra, prefix_operand[method_parameters] >> parameters \
				 , prefix_or_rule[id] >> name, optional[generic_parameters] >> gp, nothing, next[i] ^ newGenericMethod([att, in_att], mod, name, gp, parameters, ra, type, body)

	constructor_method = ~""

	here = $(success(input, input))
	next[i] = $(success((i as OMetaInput).Tail, (i as OMetaInput).Tail))
	
	method_body = Pair(	Left: _ >> newInput, Right: (block >> body)), $(success(newInput, body))
	
	method_parameters = Brackets(Type: BracketsType.Parenthesis, Form: (method_parameter_list | ((_ >> p and (p is null)) ^ [[], null])) >> p) ^ p
	
	optional_parameters = method_parameters | ("" ^ [[], null])

	method_parameter_list = (parameter >> p ^ [[p], null]) \
							| (param_array >> pa ^ [null, pa]) \
							| (Tuple(Forms: (++parameter >> p, (param_array | "") >> pa, ~_)) ^ [p, pa])
			
	parameter = --attributes_line >> att, here >> i, inline_attributes >> in_att, optional_type >> type, id >> name, next[i] ^ newParameterDeclaration([att, in_att], name, type)
	param_array = --attributes_line >> att, inline_attributes >> in_att, optional_array_type >> type, prefix[STAR], id >> name ^ newParameterDeclaration([att, in_att], name, type)
	
	optional_array_type = (Infix(Operator: AS, Left: _ >> newInput, Right: type_reference_array >> e), $(success(newInput, e)) ) | ""
	
	method_result_attributes = (Prefix(Operator: _ >> newInput, Operand: inline_attributes >> attr and (len(attr) > 0)), $(success(newInput, attr))) | ""

	generic_parameters = brackets, optional_prefix[OF], generic_parameter_list >> parameters ^ parameters

	generic_parameter_list = (generic_parameter >> p ^ [p]) \
							| (Tuple(Forms: (++generic_parameter >> p, nothing)) ^ p)

	generic_parameter = here >> i, (prefix_or_rule[id] >> name, optional[generic_parameter_constraints] >> genericParameterConstraints) \
						, next[i] ^ newGenericParameterDeclaration(name, genericParameterConstraints)
	generic_parameter_constraints = parentheses, generic_parameter_constraint_list
	
	generic_parameter_constraint_list = (generic_parameter_constraint >> c ^ [c]) | (Tuple(Forms: ++generic_parameter_constraint >> c) ^ c)
	
	generic_parameter_constraint = ( (CLASS | STRUCT | CONSTRUCTOR) >> constraint ^ newGenericParameterConstraint(constraint) ) | type_reference

	assembly_attribute = Brackets(Type: BracketsType.Square,
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
									Type: BracketsType.Square, 
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

	inline_attributes = inline_attributes_prescan | ("" ^ [])
							
	inline_attributes_prescan = (Prefix(Operator: attributes_group >> l, Operand: (inline_attributes_prescan >> r, _ >> newInput)), $(success(newInput, prepend(l, r))) )\
							| (Prefix(Operator: attributes_group >> l, Operand: (~inline_attributes_prescan, _ >> newInput)), $(success(newInput, l)) )\
							| attributes_group

	attributes_line =  Prefix(Operator: attributes_group >> l, Operand: attributes_line >> r) ^ prepend(l, r) | (attributes_group >> a ^ a)
	attributes_group = Brackets(Type:BracketsType.Square, Form: attribute_list >> attrs) ^ attrs
	
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
				, optional_type >> type \
				, id >> name, next[i] ^ newField([att, in_att], mod, name, type, initializer)
				
	field_initializer = (Infix(Operator: ASSIGN, Left: _ >> newInput, Right: (assignment | block_expression) >> e), $(success(newInput, e)) )| ""
	
	optional_type = (Infix(Operator: AS, Left: _ >> newInput, Right: type_reference >> e), $(success(newInput, e)) )| ""
	
	id = Identifier(Name: _ >> name, IsKeyword: _ >> k and (k == false), IsSymbol: _ >> s and (s == false)) ^ name
	
	identifier[n] = Identifier(Name: _ >> name and (name == n)) ^ name
	
	qualified_name = (Infix(Operator: DOT, Left: qualified_name >> l, Right: id >> r) ^ ("$l.$r")) | id
	
	stmt_line = stmt_unpack \
				| stmt_declaration \
				| stmt_expression \
				| stmt_return \
				| stmt_try \
				| stmt_yield \
				| stmt_raise \
				| stmt_macro


	
	stmt_expression = (here >> i, optional_prefix_operand[stmt_modifier] >> mod, assignment >> a, next[i] ^ ExpressionStatement(Expression: a, Modifier: mod)) \
						| stmt_expression_block \
						| ((block_expression >> e) ^ ExpressionStatement(Expression: e))
	
	stmt_expression_block = Infix(
												Operator: (ASSIGN | ASSIGN_INPLACE) >> op,
												Left: expression >> l,
												Right: block_expression >> r
										) ^ ExpressionStatement(newInfixExpression(op, l, r))
	
	block_expression = invocation_with_block | closure_block | dsl_friendly_invocation
	
	closure_block = here >> i, multiline_pair_block >> body, \
										 closure_block_left >> parameters, next[i] ^ newBlockExpression(null, null, parameters, body)

	invocation_with_block = here >> i, \
							(
								(prefix[invocation] >> e, closure_block >> be) 
								| (multiline_pair_block >> b, invocation >> e)
							), next[i] ^ newInvocationWithBlock(e, (be if be is not null else newBlockExpression(null, null, [[], null], b)))

	dsl_friendly_invocation = here >> i, multiline_pair_block >> body, member_reference_left >> mr, reference >> target, next[i]\
						^ newInvocation(getTarget(mr, target), [BlockExpression(Body: body)], null)

	multiline_pair_block = Pair(IsMultiline: _ >> ml and (ml == true), Right: block >> body, Left: _ >> newInput), $(success(newInput, body))

	block_expression_left = closure_block_left
		
	closure_block_left = ((prefix[DEF] | prefix[DO]), method_parameters >> parameters) ^ parameters \
						| ( (DEF | DO) ^ [[], null])
	
	stmt_block = stmt_if | stmt_for | stmt_while | stmt_unless
	
	stmt_unpack = here >> i, prefixOrInfix, Infix(Operator: ASSIGN, Left: declaration_list >> declarations, Right: rvalue >> e), optional[stmt_modifier] >> m, next[i] ^ newUnpackStatement(declarations, e, m)
	
	atom = reference \
			| array_literal \
			| list_literal \
			| boolean \
			| literal \
			| type_literal \
			| hash_literal \
			| parenthesized_expression \
			| quasi_quote \
			| splice_expression \
			| closure
	
	literal = (Literal(Value: _ >> f and (f isa string), Value: booparser_string_interpolation >> si) ^ si) | (Literal() >> l ^ (l as Literal).astLiteral)

	type_literal = prefix[TYPEOF], parentheses, type_reference >> type ^ newTypeofExpression(type)

	hash_literal = Brackets(
						Type: BracketsType.Curly,
						Form: optional_expression_pair_list >> items
					) ^ newHashLiteral(items)

	optional_expression_pair_list = Tuple(Forms: (++expression_pair >> a, ~_)) ^ a \
									| (expression_pair >> a ^ [a])\
									| ((_ >> e and (e == null)) ^ [])
	
	expression_pair = Pair(IsMultiline: _ >> m and (m == false), Left: assignment >> first, Right: assignment >> second) ^ ExpressionPair(First: first, Second: second)

	integer = Literal(Value: _ >> v and (v isa long)) >> l ^ (l as Literal).astLiteral
	
	splice_expression = prefix[SPLICE_BEGIN], atom >> e ^ SpliceExpression(Expression: e)
	
	booparser_string_interpolation = $(callExternalParser("string_interpolation", "Boo.OMeta.Parser.BooParser", input)) \
									| $(callExternalParser("string_literal", "Boo.OMeta.Parser.BooParser", input))
	
	closure	= Brackets( 
						Type: BracketsType.Curly,
						Form: (
								Infix(Operator: CLOSURE_SEPARATOR, Left: parameter >> p, Right: closure_stmt_list >> e) \
								| (closure_stmt_list >> e)
						)
				) ^ newBlockExpression(null, null, [([]if p is null else [p]),null], newBlock(null, null, e, null))
	
	closure_stmt_list = (closure_stmt >> s ^ [s]) | (Block(Forms: ++closure_stmt >> s) ^ s)
	
	closure_stmt = stmt_unpack \
					| closure_stmt_expression \
					| stmt_return \
					| stmt_raise \
					| closure_stmt_macro

	closure_stmt_expression = here >> i, (assignment >> e | (prefix[assignment] >> e, stmt_modifier >> m) ), next[i] ^ ExpressionStatement(Expression: e, Modifier: m)

	closure_stmt_macro = here >> i, prefix_or_rule[macro_id] >> name, optional_prefix_or_rule[assignment_list] >> args\
						, optional[stmt_modifier] >> mod, next[i] ^ newMacro(name, args, null, mod)

	array_literal = array_literal_multi | array_literal_multi_typed
	
	array_literal_multi = Brackets(
									Type: BracketsType.Parenthesis, 
									Form: ( 
										Tuple(Forms: (++assignment >> items, ~_) )
									)
							) \
							| Tuple(Forms: (++assignment >> items, ~_) ) ^ newArrayLiteral(null, items)

	array_literal_multi_typed = Brackets(
									Type: BracketsType.Parenthesis, 
									Form: ( prefix[OF], Pair(Left: type_reference >> type, Right: Tuple(Forms: (++assignment >> items, ~_) ))
									)
								) ^ newArrayLiteral(ArrayTypeReference(ElementType: type, Rank: null), items)
	
	array_literal_type = Prefix(Operator: OF, Operand: type_reference >> type) ^ ArrayTypeReference(ElementType: type, Rank: null)
	
	array_literal_multi_items = (++assignment >> a, ~_) ^ a

	list_literal = Brackets(Type: BracketsType.Square,
								Form: (
									Tuple(Forms: ((++assignment >> a, ~_) ^ a) >> items)
									| (assignment >> a ^ [a]) >> items
									| ((_ >> e and (e == null)) ^ []) >> items
								)
					) ^ newListLiteral(items)

	boolean = true_literal | false_literal
	
	true_literal = TRUE ^ [| true |]
	false_literal = FALSE ^ [| false |]
	
	parenthesized_expression = Brackets(Type: BracketsType.Parenthesis, Form: assignment >> e) ^ e
	
	binary_operator = OR | AND | ASSIGN_INPLACE | ASSIGN | IN | NOT_IN | IS | IS_NOT | PLUS | MINUS | STAR \
					| DIVISION | BITWISE_SHIFT_LEFT | BITWISE_SHIFT_RIGHT | GREATER_THAN_EQ | GREATER_THAN \
					| LESS_THAN_EQ | LESS_THAN | EQUALITY | INEQUALITY | MODULUS | BITWISE_AND | BITWISE_OR \
					| XOR
	
	binary_expression = Infix(Operator: binary_operator >> op, Left: assignment >> l, Right: (assignment >> r)) ^ newInfixExpression(op, l, r)
	
	reference = id >> r ^ ReferenceExpression(Name: r)
	
	assignment =  expression | or_expression
	
	expression = generator_expression | conditional_expression | or_expression
	
	generator_expression = here >> i, prefix[assignment] >> projection, ++generator_expression_body >> body, nothing, next[i] ^ newGeneratorExpression(projection, body)
	
	generator_expression_body = prefix[FOR], prefix_or_rule[generator_expression_body_body] >> body	\
								, optional_prefix_or_rule[filter]>> f ^ newGeneratorExpressionBody((body as List)[0], (body as List)[1], f)
								
	generator_expression_body_body = Infix(Operator: IN, Left: declaration_list >> dl, Right: rvalue >> r) ^ [dl, r]

	declaration_list = Tuple(Forms: (--declaration >> l)) | ((declaration >> l ^ [l]) >> l) ^ l

	filter = prefix[stmt_modifier_type] >> t, prefix_or_rule[or_expression] >> e ^ newStatementModifier(t, e)
	
	conditional_expression = Brackets(
								Type: BracketsType.Parenthesis,
								Form: (
									prefix[or_expression] >> trueValue
									, prefix[IF]
									, (prefix[conditional_expression] | prefix[or_expression]) >> condition
									, prefix[ELSE]
									, (conditional_expression | or_expression) >> falseValue
								)	
							) ^ newConditionalExpression(condition, trueValue, falseValue)
	
	// or expression is any operator, prefix or atom except infix ASSIGN
	or_expression = binary_expression \
					| try_cast \
					| cast_operator \
					| prefix_expression \
					| suffix_expression \
					| invocation \
					| atom \
					| member_reference \
					| isa_expression
	
	isa_expression = Infix(Operator: ISA >> op, Left: assignment >> e, Right: type_reference >> type) ^ newInfixExpression(op, e, newTypeofExpression(type))

	try_cast = Infix(Operator: AS, Left: assignment >> e, Right: type_reference >> typeRef)  ^ TryCastExpression(Target: e, Type: typeRef)
	
	cast_operator = Infix(Operator: CAST, Left: assignment >> e, Right: type_reference >> typeRef)  ^ CastExpression(Target: e, Type: typeRef)

	stmt_declaration = (typed_declaration >> d
						| Infix(Operator: ASSIGN, Left: typed_declaration >> d, Right: (assignment | block_expression) >> e)) ^ newDeclarationStatement(d, e)
	
	typed_declaration = Infix(Operator: AS, Left: Identifier(Name: _ >> name), Right: type_reference >> typeRef) ^ newDeclaration(name, typeRef)
	
	declaration = optional_type >> typeRef, id >> name ^ newDeclaration(name, typeRef)		

	prefix_expression = Prefix(IsPostfix: _ >> p and (p == false), Operator: prefix_operator >> op, Operand: assignment >> e) ^ newPrefixExpression(op, e)
	prefix_operator = NOT | MINUS | INCREMENT | DECREMENT | STAR | ONES_COMPLEMENT

	suffix_expression = Prefix(IsPostfix: _ >> p and (p == true), Operator: suffix_operator >> op, Operand: assignment >> e) ^ addSuffixUnaryOperator(e, op)
	suffix_operator = INCREMENT | DECREMENT

	invocation = here >> i, (collection_initialization | invocation_expression) >> e, ~_, next[i] ^ e
	invocation_expression = here >> i, member_reference_left >> mr, prefix_operand[invocation_arguments] >> args \
								, (
									(optional_prefix_operand[generic_arguments] >> generic_args, (reference | invocation | atom) >> target) |
									(infix[OF], reference >> target, (type_reference >> generic_args ^ [generic_args]) >> generic_args)
								) \
								, next[i] ^ newInvocation(getTarget(mr, target), args, generic_args)
	
	generic_arguments = Brackets(
							Type: BracketsType.Square, 
							Form: (optional_prefix[OF], (type_reference_list | ((type_reference >> arg) ^ [arg])) >> arg)
						) ^ arg 
						
	type_reference_list = ((type_reference >> t) ^ [t]) \
						| (Tuple(Forms: (++type_reference >> t, ~_)) ^ t)
						
	def getTarget(l, r):
		return r if l is null
		return newMemberReference(l, (r as ReferenceExpression).Name)
	
	prefix_input = $(prefixInput(input))
	
	def prefixInput(input as OMetaInput):
		return success(input) if input.IsEmpty or not input.Head isa Prefix
		
		list = []
		current = input.Head
		
		while current isa Prefix:
			list.Add((current as Prefix).Operand)
			current = (current as Prefix).Operator
		
		list.Add(current)
		
		return success(OMetaInput.For(list.Reversed))
	
	member_reference_left = (Infix(Operator: DOT, Left: member_reference >> e, Right: _ >> newInput), $(success(newInput, e))) \
							| (Prefix(Operator: DOT, Operand: _ >> newInput), $(success(newInput, OmittedExpression())) ) \
							| ""
	
	member_reference = (Infix(Operator: DOT, Left: member_reference >> e, Right: id >> name) ^ newMemberReference(e, name)) \
						| (Prefix(Operator: DOT, Operand: id >> name) ^ newMemberReference(OmittedExpression(), name)) \
						| (reference | invocation | invocation_expression | atom)
	
	invocation_arguments = Brackets(
								Type: BracketsType.Parenthesis,
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
	
	collection_initialization = here >> i, (prefix_operand[initialization_list_literal] | prefix_operand[hash_literal]) >> init \
								, invocation_expression >> e \
								, next[i] ^ newCollectionInitialization(e, init)

	initialization_list_literal = Brackets(
										Type: BracketsType.Curly,
										Form: (
											(Tuple(Forms: (++(expression) >> e, ~_) ) ^ e) \
											| (expression >> e ^ [e]) \
											| ((_ >> e and (e == null)) ^ [])
										) >> items
									) ^ newListLiteral(items)

	stmt_for = here >> i, prefix[FOR], Pair(Left:
												Infix(
													Operator: IN,
													Left: declaration_list >> dl,
													Right: rvalue >> r												
												),
											Right:
												block >> body), next[i] \
									, or_block >> orBlock \
									, then_block >> thenBlock ^ newForStatement(dl, r, body, orBlock, thenBlock)
												
	rvalue = assignment_list >> items ^ newRValue(items)
						
	stmt_if = here >> i, prefix[IF] \
						, Pair(Left: assignment >> e, Right: block >> trueBlock) \
						, next[i], false_block >> falseBlock ^ newIfStatement(e, trueBlock, falseBlock)

	false_block = 	(	
						here >> i 
						, prefix[ELIF] 
						, Pair(Left: assignment >> e, Right: block >> trueBlock) 
						, next[i], false_block >> falseBlock ^ newBlock(null, null, newIfStatement(e, trueBlock, falseBlock), null)
					) | \
					(
						Pair(Left: ELSE, Right: block >> falseBlock) ^ falseBlock						
					) | \
					("" ^ null)

	stmt_try = Pair(Left: TRY, Right: block >> protectedBlock) \
			,optional_exception_handler_list >> handlers \
			,(Pair(Left: FAILURE, Right: block >> failureBlock) | "") \
			,(Pair(Left: ENSURE, Right: block >> ensureBlock) | "") \
			^ newTryStatement(protectedBlock, handlers, failureBlock, ensureBlock)

	optional_exception_handler_list = --(
											here >> i
											, (
												(prefix[EXCEPT], Pair(Left: declaration >> d, Right: block >> b))
												| Pair(Left: EXCEPT, Right: block >> b)
											)
											, next[i]
										    ^ ExceptionHandler(Block: b, Declaration: d) 
										) >> l ^ l

	stmt_while = here >> i, prefix[WHILE], Pair(Left: assignment >> e, Right: block >> body), next[i], or_block >> orBlock, then_block >> thenBlock ^ newWhileStatement(e, body, orBlock, thenBlock)
	
	stmt_unless = here >> i, prefix[UNLESS], Pair(Left: assignment >> e, Right: block >> body), next[i] ^ newUnlessStatement(e, body)	
	
	or_block = (Pair(Left: OR, Right: block >> orBlock) | "") ^ orBlock
	then_block = (Pair(Left: THEN, Right: block >> thenBlock) | "") ^ thenBlock

	stmt_macro = here >> i, prefix_or_rule[macro_id] >> name, optional_macro_block >> b, optional_prefix_operand[stmt_modifier] >> mod, optional[assignment_list] >> args \
					, nothing, next[i] ^ newMacro(name, args, b, mod)

	optional_macro_block = ( 
								Pair(
									Doc: _ >> doc,
									Left: _ >> newInput, 
									Right: (
											empty_block 
											| (multi_line_macro_block >> stmts ^ newBlock(null, null, stmts, doc))
									) >> body
								)
								, $(success(newInput, body))	
							) | ""
							
	multi_line_macro_block = Block(Forms: ++(type_member_stmt | stmt ) >> s ) ^ s

	macro_id = Identifier(Name: _ >> name, IsKeyword: _ >> k and (k == false), IsSymbol: _ >> s and (s == false)) ^ name
	
	stmt_return = here >> i, prefix_or_rule[RETURN], ( (optional_rule_or_prefix[assignment] >> e, optional[stmt_modifier] >> m, nothing) | optional[block_expression] >> e )\
					, next[i] ^ ReturnStatement(Expression: e, Modifier: m) 

	stmt_raise = here >> i, prefix[RAISE], (expression >> e | (prefix[expression] >> e, stmt_modifier >> m)), next[i] ^ RaiseStatement(Exception: e, Modifier: m)
	
	stmt_yield = here >> i, prefix[YIELD], (expression >> e | (prefix[expression] >> e, stmt_modifier >> m)), next[i] ^ YieldStatement(Expression: e, Modifier: m)
	
	stmt_modifier = prefix[stmt_modifier_type] >> t, assignment >> e ^ newStatementModifier(t, e)
	
	stmt_modifier_type = (IF ^ StatementModifierType.If) | (UNLESS ^ StatementModifierType.Unless)
	
	assignment_list = Tuple(Forms: (++assignment >> a, ~_)) ^ a | (assignment >> a ^ [a])
	optional_assignment_list = assignment_list | ""
	
	type_reference = type_reference_simple \
					| type_reference_array \
					| type_reference_splice \
					| type_reference_callable \
					| type_reference_generic
	
	type_reference_simple = qualified_name >> name ^ SimpleTypeReference(Name: name)
	
	type_reference_splice = here >> i, prefix[SPLICE_BEGIN], atom >> e, next[i] ^ SpliceTypeReference(Expression: e)
	
	type_reference_array = Brackets(Type: BracketsType.Parenthesis, Form: ranked_type_reference >> tr)  ^ tr
	
	ranked_type_reference = (type_reference >> type) | Tuple(Forms: (type_reference >> type, integer >> rank)) ^ ArrayTypeReference(ElementType: type, Rank: rank)
	
	type_reference_callable = here >> i, optional_type >> type, prefix[CALLABLE], \
								Brackets(Type: BracketsType.Parenthesis,
									Form: (
										(type_reference >> params)
										| (param_array_reference >> paramArray)
										| Tuple(
												Forms: (++type_reference >> params, (param_array_reference|"") >> paramArray, ~_)
											)									
									)								
								), next[i] ^ newCallableTypeReference((params if (params isa List) else [params]), paramArray, type)


	param_array_reference = here >> i, prefix[STAR], type_reference >> type, next[i] ^ newParameterDeclaration(null, "arg0", type)

	type_reference_generic = (here >> i, prefix[qualified_name] >> qname, generic_arguments >> args, next[i] ^ newGenericTypeReference(qname, args)) \
							| (here >> i, member_reference_left >> mr, infix[OF], reference >> target, type_reference >> arg, next[i] ^ newGenericTypeReference(getTarget(mr, target).ToString(), [arg]))

	quasi_quote = quasi_quote_member | quasi_quote_module | quasi_quote_expression | quasi_quote_stmt
	
	quasi_quote_module = Brackets(Type: BracketsType.QQ, Form: Block(Forms: (--module_member >> members, --stmt >> stmts, ~_))) ^ newQuasiquoteExpression(newModule(null, null, [], members, stmts))
	
	quasi_quote_member = Brackets(Type: BracketsType.QQ, Form: Block(Forms: (class_member >> e, ~_))) ^ newQuasiquoteExpression(e)
	
	quasi_quote_expression = Brackets(Type: BracketsType.QQ, Form: assignment >> e) ^ newQuasiquoteExpression(e)
	
	quasi_quote_stmt = Brackets(Type: BracketsType.QQ, Form: (qq_return | qq_macro) >> e) ^ newQuasiquoteExpression(e)
	
	qq_return = (RETURN | Prefix(Operator: RETURN, Operand: assignment >> e)) ^ ReturnStatement(Expression: e, Modifier: null)
	qq_macro = prefix[id] >> name, optional_assignment_list >> args ^ newMacro(name, args, null, null) 

	#region "auxiliary rules"

	prefix[rule] = Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))
	optional_prefix[rule] = (Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))) | ""
	
	prefix_operand[rule] = Prefix(Operator: _ >> newInput, Operand: rule >> e), $(success(newInput, e))
	optional_prefix_operand[rule] = (Prefix(Operator: _ >> newInput, Operand: rule >> e), $(success(newInput, e))) | ""
	
	optional_prefix_or_rule[rule] = (Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))) | optional[rule]
	
	optional_rule_or_prefix[rule] = rule | (Prefix(Operator: (rule >> e), Operand: _ >> newInput), $(success(newInput, e))) | ""
	
	prefix_or_rule[rule] = (Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))) | rule
	
	rule_or_prefix[rule] = (Prefix(Operator: rule >> e, Operand: _ >> newInput), $(success(newInput, e))) | rule
	
	optional[rule] = (rule >> e, $(success(input, e))) | ""
	
	prefixOrId = id \
					|(
							Prefix(Operator: id >> e, Operand: _ >> newInput), $(success(newInput, e))
					)
	prefixOrInfix = (Prefix(Operator: _ >> e, Operand: _ >> newInput), $(success(OMetaInput.For([e, newInput]), e)) ) \
					| (Infix() >> e, $(success(e, e)))
					
	infix[operator] = Infix(Operator: operator >> o, Left: _ >> left, Right: _ >> right), $(success(OMetaInput.For([left, right]), o))
					
	parentheses = Brackets(Type: BracketsType.Parenthesis, Form: _ >> newInput), $(success(newInput))
	brackets = Brackets(Type: BracketsType.Square, Form: _ >> newInput), $(success(newInput))
	
	#endregion
	
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

		return FailedMatch(input, RuleFailure(id, PredicateFailure(parser)))

def success(input, value):
	return SuccessfulMatch(input, value) if input isa OMetaInput
	return SuccessfulMatch(OMetaInput.Singleton(input), value)

def success(input):
	return success(input, null)
	
def fail(input as OMetaInput, reason as RuleFailure):
	return FailedMatch(input, reason)