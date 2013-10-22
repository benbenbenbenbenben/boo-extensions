namespace Boo.TinyAst

import Boo.OMeta
import Boo.TinyAst
import Boo.Lang.PatternMatching
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler.Steps

class TinyAstParserStep(AbstractCompilerStep):
	override def Run():
		for input in Parameters.Input:
			using reader=input.Open():
				m = parseModule(reader.ReadToEnd())
				m.Name = input.Name
				CompileUnit.Modules.Add(m)
				
	def parseModule(code as string) as Module:		
		input = OMetaInput.For(code)
		parser = TinyAstParser()
		
		
		match OMetaEvaluationContextImpl(parser).Eval('module', input):
			case SuccessfulMatch(Input: OMetaInput(), Value):
				return Value
				
