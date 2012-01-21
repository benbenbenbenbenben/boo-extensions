namespace Boo.TinyAst

import Boo.Lang.Compiler
import Boo.Adt
import Boo.Lang.Compiler.Ast
import Boo.Lang.PatternMatching

macro ast:
"""
Produces a complete data type hierarchy from a declaration using Boo.Adt.DataMacro
It also produces visitor interface IAstVisitor for generated data

Example:
	ast Form = Identifier(Name as string) | Brackets(Form, Kind as BracketType)

Generated visitor:
	interface IFormVisitor:
		def OnIdentifier(node as Identifier)
		def OnBrackets(node as Brackets)
"""
	AstDataMacroExpansion(ast)
	AstMacroExpansion(ast)

public class AstDataMacroExpansion:
	
	_module as TypeDefinition
	_baseType as TypeDefinition
	
	def constructor(node as MacroStatement):
		
		assert 1 == len(node.Arguments)
		_module = enclosingModule(node)
		
		match node.Arguments[0]:
			case [| $left = $right |]:
				_baseType = createBaseType(left)
				expandDataConstructors(right)
				
			case ctor=MethodInvocationExpression():
				_baseType = [|
					class object:
						pass
				|]
				expandDataConstructor(ctor)
				
		
	def createBaseType(node as Expression):
		type = baseTypeForExpression(node)
		registerType(type)
		return type
		
	def abstractType(name as string):
		visitorType = "I$(name)Visitor"
		type = [|
			partial abstract class $name:
				abstract def Accept(visitor as $visitorType):
					pass
					
				def ToCodeString():
					writer = System.IO.StringWriter()
					Accept(TinyAstPrinterVisitor(writer))
					return writer.ToString()		
		|]
		return type
		
	def baseTypeForExpression(node as Expression):
		match node:
			case ReferenceExpression(Name: name):
				return abstractType(name)
				
			case [| $(ReferenceExpression(Name: name)) < $(ReferenceExpression(Name: baseType)) |]:
				type = [|
					partial abstract class $name($baseType):
						pass
				|]
				return type
				
			case mie=MethodInvocationExpression(Target: ReferenceExpression(Name: name)):
				type = abstractType(name)
				fields = fieldsFrom(mie)
				type.Members.Extend(fields)
				type.Members.Add(constructorForFields(fields))
				return type
				
			case gre=SlicingExpression(
						Target: ReferenceExpression(Name: name)):
				type = [|
					partial abstract class $name[of T]:
						pass
				|]
				type.GenericParameters.Clear()
				for index in gre.Indices:
					match index:
						case Slice(
							Begin: ReferenceExpression(Name: name),
							End: null,
							Step: null):
							type.GenericParameters.Add(
								GenericParameterDeclaration(Name: name))
				return type
			
		
	def expandDataConstructors(node as Expression):
		match node:
			case [| $left | $right |]:
				expandDataConstructors(left)
				expandDataConstructors(right)
			case MethodInvocationExpression():
				expandDataConstructor(node)
				
	def expandDataConstructor(node as MethodInvocationExpression):
		type = dataConstructorTypeForExpression(node.Target)
		type.LexicalInfo = node.LexicalInfo
		type.Members.Extend(fields = fieldsFrom(node))
		type.Members.Add(toStringForType(type))
		type.Members.Add(equalsForType(type))
		type.Members.Add(acceptForType(type))
		
		ctor = constructorForFields(fields)
		if len(_baseType.Members):
			superInvocation = [| super() |]
			i = 0
			for field as Field in fieldsOf(_baseType):
				ctor.Parameters.Insert(i++, 
					ParameterDeclaration(Name: field.Name, Type: field.Type))
				superInvocation.Arguments.Add(ReferenceExpression(field.Name))
			ctor.Body.Insert(0, superInvocation)
		type.Members.Add(ctor)
		
		registerType(type)
		
	def acceptForType(type as ClassDefinition):
		visitorType  = "I$(_baseType.Name)Visitor"
		visitorMethod = "On$(type.Name)"
		method = [|
			override def Accept(visitor as $visitorType):
				visitor.$visitorMethod(self)		
		|]
		return method
		
	def dataConstructorTypeForExpression(node as Expression):
		match node:
			case ReferenceExpression(Name: name):
				type = [|
					partial class $name($_baseType):
						pass
				|]
				for arg in _baseType.GenericParameters:
					type.GenericParameters.Add(
								GenericParameterDeclaration(Name: arg.Name))
				return type
						
		
	def equalsForType(type as TypeDefinition):
		
		allFields = List(fieldsIncludingBaseType(type))
		if len(allFields) == 0:
			method = [|
				override def Equals(o):
					if o is null: return false
					return self.GetType() is o.GetType()
			|]
			return method
			
		method = [|
			override def Equals(o):
				if o is null: return false
				if self.GetType() is not o.GetType(): return false
				other as $type = o
		|]
	
		for field as Field in allFields:
			comparison = [|
				if self.$(field.Name) != other.$(field.Name):
					return false
			|]
			method.Body.Add(comparison)
			
		method.Body.Add([| return true |])
		return method

	def toStringForField(field as Field):
		match field.Type:
			case ArrayTypeReference():
				yield [| "[" |]
				yield [| join(self.$(field.Name), ', ') |]
				yield [| "]" |]
			otherwise:
				yield [| self.$(field.Name) |]
		
	def fieldsIncludingBaseType(type as TypeDefinition):
		return cat(fieldsOf(_baseType), fieldsOf(type))
		
	def fieldsOf(type as TypeDefinition):
		return type.Members.Select(NodeType.Field)
		
	def constructorForFields(fields as (Field)):
		ctor = [|
			def constructor():
				pass
		|]
		for f in fields:
			ctor.Parameters.Add(p=ParameterDeclaration(Name: f.Name, Type: f.Type))
			ctor.Body.Add([| self.$(f.Name) = $p |])
		ctor.Parameters.HasParamArray = len(fields) > 0 and fields[-1].ContainsAnnotation("*")
		return ctor
		
	def fieldsFrom(node as MethodInvocationExpression):
		return array(fieldFrom(expression) for expression in node.Arguments)
	
	def fieldFrom(node as Expression):
		match node:
			case [| *$(ReferenceExpression(Name: name)) |]:
				field = fieldWith(name, ArrayTypeReference(baseTypeRef()))
				field.Annotate("*")
				return field
			case [| $(ReferenceExpression(Name: name)) as $type |]:
				return fieldWith(name, type)
			case ReferenceExpression(Name: name):
				return fieldWith(name, baseTypeRef())
		
	def baseTypeRef():
		return TypeReference.Lift(_baseType)
				
	def fieldWith(name as string, type as TypeReference):
		if name.StartsWith("@"): // mutable field
			return [|
				public $(fieldName(name)) as $type
			|]
		return [|
			public final $name as $type
		|]
				
	def fieldName(name as string):
		return (name[1:] if name.StartsWith("@") else name)
		
	def registerType(type as TypeDefinition):
		_module.Members.Add(type)



	def toStringForType(type as TypeDefinition):
		return [|
			override def ToString():
				return ToCodeString()
		|]

public class AstMacroExpansion:
	
	_module as TypeDefinition
	
	def constructor(node as MacroStatement):
		
		assert 1 == len(node.Arguments)
		_module = enclosingModule(node)
		
		match node.Arguments[0]:
			case [| $left = $right |]:
				createVisitorInterface(left,right)
		
//		nodeExtension = [|
//				[Extension]
//				static def ToCodeString(this as $left):
//					writer = System.IO.StringWriter();
//					this.Accept(TinyAstPrinterVisitor(writer));
//					return writer.ToString();
//		|]
//		
//		registerType(nodeExtension)

	def createVisitorInterface(left as Expression, right as Expression):
		match left:
			case ReferenceExpression(Name: name)
		name  = "I${name}Visitor"
		type = [|
			interface $name:
				pass
		|]
		
		expandInterfaceMethods(right, type)		
		registerType(type)
		return type

	def expandInterfaceMethods(node as Expression, visitorInterface as InterfaceDefinition):
		match node:
			case [| $left | $right |]:
				expandInterfaceMethods(left, visitorInterface)
				expandInterfaceMethods(right, visitorInterface)
			case MethodInvocationExpression():
				expandInterfaceMethod(node, visitorInterface)
				
	def expandInterfaceMethod(node as MethodInvocationExpression, visitorInterface as InterfaceDefinition):
		match node.Target:
			case ReferenceExpression(Name: name):
				methodName = "On${name}"
				type = [|
				 	interface i:
				 		def $methodName(node as $name)
				 	
				|]				
				visitorInterface.Members.Add(type.Members[0])				
	
	def registerType(type as TypeMember):
		_module.Members.Add(type)
	
	def enclosingModule(node as Node) as Module:
		return node.GetAncestor(NodeType.Module)

