namespace Boo.OMeta.Parser

import System.Globalization
import Boo.OMeta
import Boo.Lang.PatternMatching
import Boo.Lang.Compiler
import Boo.Lang.Compiler.Ast

def newMacro(input as OMetaInput, name, args, body, m):
	node = MacroStatement(Name: tokenValue(name), Body: body, Modifier: m, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for arg in args: node.Arguments.Add(arg)
	return node

def newSlicing(input as OMetaInput, target as Expression, slices):
	node = SlicingExpression(Target: target, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for slice in slices: node.Indices.Add(slice)
	return node
	
def newSlice(input as OMetaInput, begin as Expression, end as Expression, step as Expression):
	return Slice(begin, end, step, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))

def newRValue(input as OMetaInput, items as List):
	if len(items) > 1: return newArrayLiteral(input, items)
	return items[0]

def newForStatement(input as OMetaInput, declarations, e as Expression, body as Block):
	node = ForStatement(Iterator: e, Block: body, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for d in declarations: node.Declarations.Add(d)
	return node

#def newDeclaration(name, type as TypeReference):
#	return Declaration(Name: tokenValue(name), Type: type)
def newDeclaration(input as OMetaInput, name, type as TypeReference):
	return Declaration(Name: tokenValue(name), Type: type, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))

def newDeclarationStatement(input as OMetaInput, d as Declaration,  initializer as Expression):
	return DeclarationStatement(Declaration: d, Initializer: initializer, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newUnpackStatement(input as OMetaInput, declarations, e as Expression, m as StatementModifier):
	stmt = UnpackStatement(Expression: e, Modifier: m, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for d in declarations: stmt.Declarations.Add(d)
	return stmt

def newIfStatement(input as OMetaInput, condition as Expression, trueBlock as Block):
	return IfStatement(Condition: condition, TrueBlock: trueBlock, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newCallable(input as OMetaInput, name, parameters, type):
	node = CallableDefinition(Name: tokenValue(name), ReturnType: type, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	setUpParameters node, parameters
	return node
	
def newModule(input as OMetaInput, doc, imports, members, stmts):
	m = Module(Documentation: doc, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for item in imports: m.Imports.Add(item)
	for member in flatten(members):
		if member isa Attribute:
			m.AssemblyAttributes.Add(member)
		else:
			m.Members.Add(member)
	for stmt as Statement in stmts: m.Globals.Add(stmt)
	return m
	
def newImport(input as OMetaInput, qname as string):
	return Import(Namespace: qname, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))

def newInteger(input as OMetaInput, t, style as NumberStyles):
	value = int.Parse(tokenValue(t), style)
	return IntegerLiteralExpression(Value: value, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newEvent(input as OMetaInput, attributes, modifiers, name, type):
	return setUpMember(Event(Name: tokenValue(name), Type: type, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position)), attributes, modifiers)
	
def newField(input as OMetaInput, attributes, modifiers, name, type, initializer):
	return setUpMember(Field(Name: tokenValue(name), Type: type, Initializer: initializer, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position)), attributes, modifiers)
	
def newProperty(input as OMetaInput, attributes, modifiers, name, parameters, type, getter, setter):
	node = Property(Name: tokenValue(name), Type: type, Getter: getter, Setter: setter, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	setUpParameters node, parameters
	return setUpMember(node, attributes, modifiers)
	
def setUpAttributes(node as INodeWithAttributes, attributes) as Node:
	for a in flatten(attributes):
		node.Attributes.Add(a)
	return node
	
def setUpMember(member as TypeMember, attributes, modifiers):
	setUpAttributes member, attributes
	for m as TypeMemberModifiers in flatten(modifiers): member.Modifiers |= m
	return member
	
def setUpParameters(node as INodeWithParameters, parameters):
	for p in flatten(parameters): node.Parameters.Add(p)
	
def newMethod(input as OMetaInput, attributes, modifiers, name, parameters, returnTypeAttributes, returnType as TypeReference, body as Block) as Method:
	node = Method(Name: tokenValue(name), Body: body, ReturnType: returnType, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	setUpParameters node, parameters
	for a in flatten(returnTypeAttributes): node.ReturnTypeAttributes.Add(a)
	return setUpMember(node, attributes, modifiers)
	
def newGenericMethod(input as OMetaInput, attributes, modifiers, name, genericParameters, parameters, returnTypeAttributes, returnType as TypeReference, body as Block):
	node = newMethod(input, attributes, modifiers, name, parameters, returnTypeAttributes, returnType, body)
	for gp in flatten(genericParameters): node.GenericParameters.Add(gp)
	return node

def newGenericTypeReference(input as OMetaInput, qname, args):
	node = GenericTypeReference(Name: qname, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for arg in flatten(args): node.GenericArguments.Add(arg)
	return node
	
def newGenericParameterDeclaration(input as OMetaInput, name):
	node = GenericParameterDeclaration(Name: tokenValue(name), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	return node
	
def newParameterDeclaration(input as OMetaInput, attributes, name, type):
	node = ParameterDeclaration(Name: tokenValue(name), Type: type, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	return setUpAttributes(node, attributes)
	
def newEnum(input as OMetaInput, attributes, modifiers, name, members):
	return setUpType(EnumDefinition(Name: tokenValue(name)), attributes, modifiers, null, members)
	
def newCallableTypeReference(input as OMetaInput, params, type):
	node = CallableTypeReference(ReturnType: type, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	i = 0
	for p in flatten(params):
		node.Parameters.Add(ParameterDeclaration(Name: "arg${i++}", Type: p, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position)))
	return node
	
def newStatementModifier(input as OMetaInput, t, e as Expression):
	return StatementModifier(Type: t, Condition: e, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newGeneratorExpressionBody(input as OMetaInput, dl, e, f):
	node = GeneratorExpression(Iterator: e, Filter: f, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for d in flatten(dl): node.Declarations.Add(d)
	return node
	
def newGeneratorExpression(input as OMetaInput, projection, body as List):
	node as GeneratorExpression = body[0]
	node.Expression = projection
	if len(body) == 1: return node
	
	e = ExtendedGeneratorExpression(LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for item in body: e.Items.Add(item)
	return e
	
def newEnumField(input as OMetaInput, attributes, name, initializer):
	match initializer:
		case [| -$(e=IntegerLiteralExpression()) |]:
			e.Value *= -1
			initializer = e
		otherwise:
			pass
	return setUpMember(EnumMember(Name: tokenValue(name), Initializer: initializer, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position)), attributes, null)
	
def newClass(input as OMetaInput, attributes, modifiers, name, baseTypes, members):
	return setUpType(ClassDefinition(Name: tokenValue(name), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position)), attributes, modifiers, baseTypes, members)
	
def setUpType(type as TypeDefinition, attributes, modifiers, baseTypes, members):
	if members is not null: 
		for member in members: type.Members.Add(member)
	if baseTypes is not null:
		for baseType in baseTypes: type.BaseTypes.Add(baseType)
	return setUpMember(type, attributes, modifiers)
	
macro setUpArgs:
	node, args = setUpArgs.Arguments
	code = [|
		if $args is not null:
			for arg in $args:
				if arg isa ExpressionPair:
					$node.NamedArguments.Add(arg)
				else:
					$node.Arguments.Add(arg)
	|]
	return code
	
def newAttribute(input as OMetaInput, name, args):
	node = Attribute(Name: tokenValue(name), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	setUpArgs node, args
	return node
	
def newNamedArgument(input as OMetaInput, name, value):
	return ExpressionPair(First: newReference(input, name), Second: value, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newInterface(input as OMetaInput, attributes, modifiers, name, baseTypes, members):
	return setUpType(InterfaceDefinition(Name: tokenValue(name), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position)), attributes, modifiers, baseTypes, members)
	
def newInvocation(input as OMetaInput, target as Expression, args as List):
	mie = MethodInvocationExpression(Target: target, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	setUpArgs mie, args
	return mie
	
def newQuasiquoteBlock(input as OMetaInput, m):
	return QuasiquoteExpression(Node: m, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newQuasiquoteExpression(input as OMetaInput, s):
	return QuasiquoteExpression(Node: s, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newReference(input as OMetaInput, t):
	return ReferenceExpression(Name: tokenValue(t), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newMemberReference(input as OMetaInput, target as Expression, name):
	return MemberReferenceExpression(Target: target, Name: tokenValue(name), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newArrayLiteral(input as OMetaInput, type, items):
	node = newArrayLiteral(input, items)
	node.Type = type
	return node
	
def newArrayLiteral(input as OMetaInput, items):
	literal = ArrayLiteralExpression(LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for item in items:
		literal.Items.Add(item)
	return literal
	
def newListLiteral(input as OMetaInput, items):
	literal = ListLiteralExpression(LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for item in items: literal.Items.Add(item)
	return literal
	
def newHashLiteral(input as OMetaInput, items):
	literal = HashLiteralExpression(LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for item in items: literal.Items.Add(item)
	return literal
	
def newStringLiteral(input as OMetaInput, s):
	return StringLiteralExpression(Value: tokenValue(s), LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newStringInterpolation(input as OMetaInput, items as List):
	if len(items) == 0: return StringLiteralExpression("", LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	if len(items) == 1 and items[0] isa StringLiteralExpression:
		return items[0]
	node = ExpressionInterpolationExpression()
	for item in items: node.Expressions.Add(item)
	return node
	
def newConditionalExpression(input as OMetaInput, condition, trueValue, falseValue):
	return ConditionalExpression(Condition: condition, TrueValue: trueValue, FalseValue: falseValue, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newBlockExpression(input as OMetaInput, parameters, body):
	node = BlockExpression(Body: body, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	for p in parameters: node.Parameters.Add(p)
	return node
	
def newTypeofExpression(input as OMetaInput, type):
	return TypeofExpression(type, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newInvocationWithBlock(input as OMetaInput, invocation as MethodInvocationExpression, block as BlockExpression):
	node = invocation.CloneNode()
	node.Arguments.Add(block)
	return node
	
def newInfixExpression(input as OMetaInput, op, l as Expression, r as Expression):
	return BinaryExpression(Operator: binaryOperatorFor(op), Left: l, Right: r, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def newPrefixExpression(input as OMetaInput, op, e as Expression):
	return UnaryExpression(Operator: unaryOperatorFor(op), Operand: e, LexicalInfo: LexicalInfo("", input.Line, input.Column, input.Position))
	
def unaryOperatorFor(op):
	match tokenValue(op):
		case "not": return UnaryOperatorType.LogicalNot
		case "-": return UnaryOperatorType.UnaryNegation
		case "~": return UnaryOperatorType.OnesComplement
		case "++": return UnaryOperatorType.Increment
		case "--": return UnaryOperatorType.Decrement
	
def binaryOperatorFor(op):
	match tokenValue(op):
		case "is": return BinaryOperatorType.ReferenceEquality
		case "is not": return BinaryOperatorType.ReferenceInequality
		case "in": return BinaryOperatorType.Member
		case "not in": return BinaryOperatorType.NotMember
		case "and": return BinaryOperatorType.And
		case "or": return BinaryOperatorType.Or
		case "|": return BinaryOperatorType.BitwiseOr
		case "&": return BinaryOperatorType.BitwiseAnd
		case "^": return BinaryOperatorType.ExclusiveOr
		case "+": return BinaryOperatorType.Addition
		case "-": return BinaryOperatorType.Subtraction
		case "*": return BinaryOperatorType.Multiply
		case "**": return BinaryOperatorType.Exponentiation
		case "/": return BinaryOperatorType.Division
		case "%": return BinaryOperatorType.Modulus
		case "=": return BinaryOperatorType.Assign
		case "==": return BinaryOperatorType.Equality
		case "!=": return BinaryOperatorType.Inequality
		case "+=": return BinaryOperatorType.InPlaceAddition
		case "-=": return BinaryOperatorType.InPlaceSubtraction
		case "/=": return BinaryOperatorType.InPlaceDivision
		case "*=": return BinaryOperatorType.InPlaceMultiply
		case "^=": return BinaryOperatorType.InPlaceExclusiveOr
		case "&=": return BinaryOperatorType.InPlaceBitwiseAnd
		case "|=": return BinaryOperatorType.InPlaceBitwiseOr
		case ">>": return BinaryOperatorType.ShiftRight
		case "<<": return BinaryOperatorType.ShiftLeft
		case "<": return BinaryOperatorType.LessThan
		case "<=": return BinaryOperatorType.LessThanOrEqual
		case ">": return BinaryOperatorType.GreaterThan
		case ">=": return BinaryOperatorType.GreaterThanOrEqual
	
def newAssignment(input as OMetaInput, l as Expression, r as Expression):
	return [| $l = $r |]
	
def newBlock(input as OMetaInput, contents):
	b = Block()
	match contents:
		case Statement():
			b.Statements.Add(contents)
		otherwise:
			for item in contents:
				b.Statements.Add(item)
	return b
	
def prepend(first, tail as List):
	if first is null: return tail
	return [first] + tail
	
def buildQName(q, rest):
	return join(tokenValue(t) for t in prepend(q, rest), '.')
