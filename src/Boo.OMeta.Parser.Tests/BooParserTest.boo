namespace Boo.OMeta.Parser.Tests

import Boo.OMeta
import Boo.OMeta.Parser
import Boo.Lang.PatternMatching

import Boo.Lang.Compiler.Ast

import NUnit.Framework

[TestFixture]
class BooParserTest:
	
	[Test]
	def TestSimpleModuleWithImport():
		
		code = """
import System.Console

WriteLine(42)
"""
		
		m = parseModule(code)
		Assert.AreEqual(1, len(m.Imports))
		Assert.AreEqual("System.Console", m.Imports[0].Namespace)
		
	def parseModule(code as string) as Module:
		match BooParser().module(code):
			case SuccessfulMatch(Input: OMetaInput(IsEmpty: true), Value):
				return Value
		
		
		
	[Test]
	def TestEndSourceLocationForInlineClosures():
		code = """foo = { a = 3;
return a; }"""
		EnsureClosureEndSourceLocation(code, 2, 11)
		
		
	[Test]
	def TestEndSourceLocationForBlockClosures():
		code = """
foo = def():
    return a
"""
		EnsureClosureEndSourceLocation(code, 3, 13)
		

	def EnsureClosureEndSourceLocation(code as string, line as int, column as int):		
		parser = BooParser()
		
		match parser.module(code):
			case SuccessfulMatch(Input: input, Value: m=Module()):
				assert m is not null
				assert input.IsEmpty, input.ToString()
				e = (m.Globals.Statements[0] as ExpressionStatement).Expression
				cbe = (e as BinaryExpression).Right as BlockExpression
				esl = cbe.Body.EndSourceLocation
				Assert.AreEqual(line, esl.Line)
				Assert.AreEqual(column, esl.Column)
		