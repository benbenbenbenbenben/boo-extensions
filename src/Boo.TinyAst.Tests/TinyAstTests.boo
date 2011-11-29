namespace Boo.TinyAst.Tests

import System
import System.IO
import NUnit.Framework
import System.Text
import Boo.OMeta
import Boo.TinyAst
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler.Steps
import Boo.Lang.Compiler.IO
import Boo.Adt
import Boo.OMeta.Parser

data Exp = Const(value as int) | Infix(operator as string, left as Exp, right as Exp)

ometa ExternalParser:
	eol = '\n' | "\r\n" | "\r" | ~_
	parse = --(sum >> s, eol ^ newMacroStatement(s)) >> m ^ m
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

[TestFixture]
class TinyAstTests:
	
	[Test]
	def ExternalParserIntegrationTest1():
		
		output = StringWriter()
		Console.SetOut(output)
		
		code = """
get parse of Boo.TinyAst.Tests.ExternalParser from:
(1+2)*3
(1+1)*3

print 1
"""
		o = OMetaParseAndRun(code)
		assert normalize(output.ToString()) == "9\n6\n1"

	[Test]
	def ExternalParserIntegrationTest2():
		output = StringWriter()
		Console.SetOut(output)
		
		code = """
get form_stmt of Boo.TinyAst.TinyAstParser from:
a = (1, 2, 3,)
"""
		o = OMetaParseAndRun(code)
		assert normalize(output.ToString()) == ""


	def normalize(s as string):
		return s.Trim().Replace("\r\n", "\n")
		
	def OMetaParseAndRun(code as string):
		compiler = Boo.Lang.Compiler.BooCompiler()
		compiler.Parameters.OutputWriter = StringWriter()
		compiler.Parameters.References.Add(typeof(MacroMacro).Assembly)
		compiler.Parameters.References.Add(typeof(OMetaMacroProcessor).Assembly)
		compiler.Parameters.References.Add(typeof(WhitespaceSensitiveTokenizer).Assembly)
		compiler.Parameters.References.Add(typeof(TinyAstParser).Assembly)	
		compiler.Parameters.References.Add(GetType().Assembly)
		
		p = Boo.Lang.Compiler.Pipelines.CompileToMemory()		
		p.Add(RunAssembly())		
		p.Replace(typeof(Parsing), BooParserStep())
		
		compiler.Parameters.Pipeline = p
		compiler.Parameters.Input.Add(StringInput("", code))
		return compiler.Run()

	
