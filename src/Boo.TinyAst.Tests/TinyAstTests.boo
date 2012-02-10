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
import Boo.Lang.PatternMatching

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
		OMetaParseAndRun(code)
		assert normalize(output.ToString()) == "9\n6\n1"

	[Test]
	def ExternalParserIntegrationTest2():
		output = StringWriter()
		Console.SetOut(output)

		code = """
get form of Boo.TinyAst.TinyAstParser from:
a0 as (int,1)
"""
		OMetaParseAndRun(code)
		assert normalize(output.ToString()) == ""

	[Test]
	def Tuple1():
		TestParseBlock("""1, 2, 3, 4""")

	[Test]
	def Tuple2():
		TestParseBlock("""(1, 2), (3, 4)""")
		
	[Test]
	def Infix1():
		TestParseBlock("""names = (
			"Tex",
			"Nanico",
			"Bamboo"
		)""")
		
	[Test]
	def Prefix1():
		TestParseBlock("""\$(extension)""")

	[Test]
	def Prefix2():
		TestParseBlock("""\$(extension)(context)""")

	[Test]
	def Prefix3():
		TestParseBlock("""for (n, a) in zip(names, attributes)""")

	[Test]
	def MultilinePair1():
		TestParseBlock("""for (n, a) in zip(names, attributes):
	print("\${n} \${a}!")""")


				
	private def TestParseBlock(code as string):
		parser = TinyAstParser()
		o = parser.block(code)
		match o:
			case SuccessfulMatch(Input: input):
				assert input.IsEmpty
	
	
	[Test]
	def TinyAstTest1():
		code = """for n in zip(names, attributes):
	print("mmm")"""
		code1 = """print("mmm")"""

		//a1 = [| Operator:binary_operator >> op |]
		
//		booParser = BooParser()
//		m =  booParser.module(code2)
//		bp = BooParser()
//		match bp.stmt(code):
//			case SuccessfulMatch(Value: s)
//		print s
		
		parser = TinyAstParser()
		match parser.block(code):
			case SuccessfulMatch(Value: o = Boo.TinyAst.Block())
		
		print o
		
		evaluator = TinyAstEvaluator()
		b = evaluator.invocation(OMetaInput.Singleton(o.Forms[0]))
		b = evaluator.stmt_for(OMetaInput.Singleton(o.Forms[0]))
		
		print b
		

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

	[Test]
	def GenGrammar1():
		GenGrammarCode(macroCode1)
		
	[Test]
	def GenGrammar2():
		GenGrammarCode(macroCode2)
		
	def GenGrammarCode(code):
		compiler = Boo.Lang.Compiler.BooCompiler()
		compiler.Parameters.OutputWriter = StringWriter()
		compiler.Parameters.References.Add(typeof(MacroMacro).Assembly)
		compiler.Parameters.References.Add(typeof(OMetaMacroProcessor).Assembly)
		compiler.Parameters.References.Add(typeof(Boo.TinyAst.Form).Assembly)
		compiler.Parameters.References.Add(typeof(WhitespaceSensitiveTokenizer).Assembly)        
		compiler.Parameters.References.Add(typeof(DataMacroExpansion).Assembly)
		
		compiler.Parameters.Pipeline = Boo.Lang.Compiler.Pipelines.Compile()
		compiler.Parameters.Input.Add(StringInput("macro", code))
		context = compiler.Run()
		print context.Errors
		m = context.CompileUnit.Modules[0]
		print m.ToCodeString()

	macroCode1 = """
namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.OMeta.Parser
import Boo.Lang.Compiler.Ast

ometa TinyAstEvaluator:
	array_literal_multi = Brackets(Kind: BracketType.Parenthesis, Form: Infix(Operator: boolean333 >> op2) >> op1) 
	
	boolean333 = ""
	
	assignment = (Infix(Operator: Identifier(Name: _ >> op), Left:expression >> l, Right: expression >> r) ^ newInfixExpression(Token(op, op), l, r)) \
				| atom
"""

	macroCode2 = """
namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.OMeta.Parser
import Boo.Lang.Compiler.Ast

ometa AClassMatching:
	one = "12"
	two = "23"
	example1 = A(first: _ >> f and (f isa string), first: one, second: two)
	//example1 = A(first: _ >> f, second: two >> s)
"""