namespace Boo.TinyAst.Tests

import System
import System.IO
import NUnit.Framework
import System.Text
import Boo.OMeta
import Boo.Lang.PatternMatching
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler.Steps
import Boo.Lang.Compiler.IO
import Boo.Adt
import Boo.OMeta.Parser

data Exp = Const(value as int) | Infix(operator as string, left as Exp, right as Exp)

ometa ExternalParser:
	module = (
		--parse >> stmts
	) ^ newModule(null, null, [], [], stmts)
	parse = sum >> s ^ newMacroStatement(s)
	sum = (sum >> l, ('+' | '-') >> op, fac >> r) ^ Infix(op, l, r) | fac 
	fac = (fac >> l, ('*' |  '/') >> op, atom >> r) ^ Infix(op, l, r) | atom
	atom = num | parens
	parens = ('(', sum >> value, ')') ^ value
	num = ++digit >> value ^ Const(int.Parse(join(value, '')))
	
	#converting to AST
	exp = const|infix
	const = Const(value) ^ IntegerLiteralExpression(Value: value)
	infix = Infix(operator, left:exp >> l, right:exp >> r) ^ newInfixExpression(Token("operator", operator), l, r)	
	
	def newMacroStatement(data):
		m = MacroStatement("testMacro")
		m.Annotate("someData", data)
		return m

def makeString(values):
	buffer = StringBuilder()
	for value in values:
		buffer.Append(value)
	b = buffer.ToString()
	return b

class ExternalParserStep(AbstractCompilerStep):
	override def Run():
		for input in Parameters.Input:
			using reader=input.Open():
				m = parseModule(reader.ReadToEnd())
				m.Name = input.Name
				CompileUnit.Modules.Add(m)
				
				
	def parseModule(code as string) as Module:
		
		input = OMetaInput.For(code)
		parser = ExternalParser()
		
		match OMetaEvaluationContextImpl(parser).Eval('module', input):
			case SuccessfulMatch(Input: OMetaInput(IsEmpty: true), Value):
				return Value	


[TestFixture]
class TinyAstTests:
	
	[Test]
	def ExternalParserIntegrationTest1():
		output = StringWriter()
		Console.SetOut(output)		
		OMetaParseAndRun("(1+2)*3")

		assert normalize(output.ToString()) == "9"			

	def normalize(s as string):
		return s.Trim().Replace("\r\n", "\n")
		
	def OMetaParseAndRun(code as string):
		compiler = Boo.Lang.Compiler.BooCompiler()
		compiler.Parameters.OutputWriter = StringWriter()
		compiler.Parameters.References.Add(typeof(MacroMacro).Assembly)
		compiler.Parameters.References.Add(typeof(OMetaMacroProcessor).Assembly)
		compiler.Parameters.References.Add(typeof(WhitespaceSensitiveTokenizer).Assembly)		
		compiler.Parameters.References.Add(GetType().Assembly)
		
		p = Boo.Lang.Compiler.Pipelines.CompileToMemory()		
		p.Add(RunAssembly())		
		p.Replace(typeof(Parsing), ExternalParserStep())
		
		compiler.Parameters.Pipeline = p
		compiler.Parameters.Input.Add(StringInput("", code))
		return compiler.Run()

	
