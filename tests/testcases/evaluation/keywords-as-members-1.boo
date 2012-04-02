"""
interface IFoo:

	def get() as object

	def set(value)

class Foo(IFoo):

	def get():
		return null

	def set(value):
		pass

Foo().set('')
print(Foo().get())
"""
interface IFoo:
	def get() as object
	def set(value)

class Foo(IFoo):
	def get():
		return null
	def set(value):
		pass
		
Foo().set("")
print Foo().get()

