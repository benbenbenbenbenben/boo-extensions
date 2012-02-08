namespace Boo.OMeta.Tests

import Boo.OMeta
import Boo.Lang.PatternMatching
import Boo.Adt
import NUnit.Framework

data Exp = Const(value as int) | Sum(left as Exp, right as Exp)
data Employee(FirstName as string,	LastName as string)


[TestFixture]
class ObjectMatchingTest:
	
	[Test]
	def PropertyPatternMatching():
		
		ometa ConstList:
			option ParseTree
			parse = ++(Const(value) ^ value)
			
		match ConstList().parse(OMetaInput.For([Const(1), Const(42)])):
			case SuccessfulMatch(Input, Value):
				assert Input.IsEmpty
				Assert.AreEqual([1, 42], Value)
				
	[Test]
	def PropertyParsing():
		ometa Evaluator:
			eval = const | sum
			const = Const(value) ^ value
			sum = Sum(left: eval >> l as int, right: eval >> r as int) ^ (l + r)
				
		match Evaluator().eval(OMetaInput.Singleton(Sum(Const(21), Sum(Const(11), Const(10))))):
			case SuccessfulMatch(Input, Value):
				assert Input.IsEmpty
				Assert.AreEqual(42, Value)

	[Test]
	def MatchingStringProperties():			
		ometa EmployeeClassMatching:
			firstName = "John", ~_
			lastName = "Lennon", ~_
			//example1 = A(first: _ >> f and (f isa string), first: one, second: two)
			checkJohnLennon = Employee(FirstName: firstName, LastName: lastName)
	
		match EmployeeClassMatching().checkJohnLennon(OMetaInput.Singleton(Employee("John", "Lennon"))):
			case SuccessfulMatch(Input):
				assert Input.IsEmpty
				
		match EmployeeClassMatching().checkJohnLennon(OMetaInput.Singleton(Employee("Julian", "Lennon"))):
			case FailedMatch():
				pass
				
		match EmployeeClassMatching().checkJohnLennon(OMetaInput.Singleton(Employee("John", "Travolta"))):
			case FailedMatch():
				pass
				
	[Test]
	def UsingUnderscore():			
		ometa EmployeeClassMatching2:
			getFirstName = Employee(FirstName: _ >> f) ^ f
	
		match EmployeeClassMatching2().getFirstName(OMetaInput.Singleton(Employee("John", "Lennon"))):
			case SuccessfulMatch(Value, Input):
				assert Value = "John"
				assert Input.IsEmpty

	[Test]
	def UsingPredicate():			
		ometa EmployeeClassMatching3:
			getFirstName = Employee(FirstName: _ >> f and (f isa string)) ^ f
	
		match EmployeeClassMatching3().getFirstName(OMetaInput.Singleton(Employee("John", "Lennon"))):
			case e = SuccessfulMatch(Value, Input):
				assert Value = "John"
				assert Input.IsEmpty

