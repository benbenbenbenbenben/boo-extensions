"""
override def ToString():
	pass

override def Equals(other):
	return false

override def Equals(o):
	return true

myclass(Foo, { myclass(Bar, { return }) })
"""
myclass Foo:
	override def ToString():
		pass
		
	myclass Bar:
		override def Equals(other):
			return false
			
	override def Equals(o):
		return true
