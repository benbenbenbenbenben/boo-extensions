"""
[Boo.Lang.ExtensionAttribute]
static def foo(item as string):
	return item.ToUpper()

[Boo.Lang.ExtensionAttribute]
static def join(item as string, items):
	return join(items, self)
"""
[Extension]
static def foo(item as string):
	return item.ToUpper()
	
[Extension]
static def join(item as string, items):
	return join(items, self)
