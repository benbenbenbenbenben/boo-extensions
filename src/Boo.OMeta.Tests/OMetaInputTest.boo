namespace Boo.OMeta.Tests

import Boo.OMeta
import NUnit.Framework

[TestFixture]
class OMetaInputTest:
	
#	[Test]
	def EqualsArgument():
		
		arg = "foo"
		input = OMetaInput.Empty()
		input1 = input.Prepend(arg)
		input2 = input.Prepend(arg)
		
		assert input1 == input2
		
		input3 = input.Prepend("bar")
		assert input1 != input3
		
#	[Test]
	def EqualsMemo():
		
		input = OMetaInput.For("foo")
		input1 = input.SetMemo("a", "b")
		input2 = input.SetMemo("a", "b")
		assert input1 == input2
	
	[Test]
	def Position():
		
		items = range(10)
		input = OMetaInput.For(items)
		for i in items:
			Assert.AreEqual(i, input.Position)
			input = input.Tail
		assert input.IsEmpty
		Assert.AreEqual(10, input.Position)
		
	[Test]
	def TailIsCached():
		
		input = OMetaInput.For(range(3))
		while not input.IsEmpty:
			Assert.AreSame(input.Tail, input.Tail)
			input = input.Tail
			
	[Test]
	def PrependPreservesMemo():
		input = OMetaInput.For("foo")
		input1 = input.SetMemo("a", "b")
		input2 = input1.Prepend("bar")
		assert input2.GetMemo("a") == "b"
		