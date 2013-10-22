namespace Boo.OMeta.Parser

import Boo.OMeta
import Boo.Lang.PatternMatching
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler.Steps

class BooParserStep(AbstractCompilerStep):
	override def Run():
		for input in Parameters.Input:
			using reader=input.Open():
				m = parseModule(reader.ReadToEnd())
				m.Name = input.Name
				CompileUnit.Modules.Add(m)
				
	def parseModule(code as string) as Module:		
		input = OMetaInput.For(code)
		parser = extendGrammar(BooParser(Parameters))
		
		match OMetaEvaluationContextImpl(parser).Eval('module', input):
			case SuccessfulMatch(Input: OMetaInput(IsEmpty: true), Value):
				return Value
				
	def extendGrammar(grammar as OMetaGrammar):
		for r in Parameters.References:
			assemblyRef = r as Boo.Lang.Compiler.TypeSystem.Reflection.IAssemblyReference
			continue if assemblyRef is null
			
			assembly = assemblyRef.Assembly
			for attribute as SyntaxExtensionAttribute in assembly.GetCustomAttributes(SyntaxExtensionAttribute, true):
				grammar = attribute.Type(grammar)
				
		return grammar
		
		