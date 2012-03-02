namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.TinyAst
import Boo.Lang.Environments
import Boo.Lang.Compiler
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler.Ast as AST

macro tinyAst:
	_module = enclosingModule(__macro)
	block = __macro["tinyAst"] as Block
	input = OMetaInput.For(block.Forms)
	while not input.IsEmpty:
		match TinyAstEvaluator(my(CompilerContext).Parameters).expansion(input):
			case SuccessfulMatch(Value: value, Input: input):
				if value isa List:
					for item in value: 
						if item isa AST.Attribute:
							_module.AssemblyAttributes.Add(item)
						else:
							yield item
				else:
					yield value
					
def enclosingModule(node as Node) as Module:
	return node.GetAncestor(NodeType.Module)
