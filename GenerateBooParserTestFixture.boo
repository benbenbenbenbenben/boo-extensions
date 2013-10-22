import System.IO

def testCaseNameFor(fname as string):
	return Path.GetFileNameWithoutExtension(fname).Replace("-", "_")

def writeTestFixture(writer as TextWriter):
	
	write = { o | writer.WriteLine(o) }
	
	write """namespace Boo.OMeta.Parser.Tests
	
import NUnit.Framework

[TestFixture]
partial class BooParserTestFixture:
	
	def runTestCase(fname as string):
		
		fullName = Path.Combine(booRoundtripTestCasesPath(), fname)
		
		parser = BooParser()
		match parser.module(File.ReadAllText(fullName)):
			case SuccessfulMatch(Input: input, Value: m=Module()):
				assert m is not null
				assert m.Documentation is not null
				Assert.AreEqual(normalize(m.Documentation), normalize(m.ToCodeString()))
				assert input.IsEmpty, input.ToString()



	def normalize(s as string):
		return s.Trim().Replace("\r\n", "\n")
		
	[once] def booRoundtripTestCasesPath():
		
		return Path.Combine(
					findSiblingBooDirectory(parentDirectory(System.Environment.CurrentDirectory)),
					"tests/testcases/parser/roundtrip")
		
	def findSiblingBooDirectory(dir as string) as string:
		
		booDir = Path.Combine(dir, "boo")
		if Directory.Exists(booDir): return booDir
		
		parent = parentDirectory(dir)
		assert parent != dir
		return findSiblingBooDirectory(parent)
		
	def parentDirectory(dir as string):
		return Path.GetDirectoryName(dir)	
"""

	for fname as string in Directory.GetFiles("../../boo/tests/testcases/parser/roundtrip"):
		continue unless fname.EndsWith(".boo")
		write """
	[Test]
	def ${testCaseNameFor(fname)}():
		runTestCase("${Path.GetFileName(fname)}")
	"""
		
#writeTestFixture System.Console.Out

using writer=StreamWriter("src/Boo.OMeta.Parser.Tests/BooParserTestFixture.Generated.boo"):
	writeTestFixture writer