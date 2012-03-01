namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.TinyAst
import Boo.Lang.Environments
import Boo.Lang.Compiler

macro tinyAst:
	block = __macro["tinyAst"] as Block
	input = OMetaInput.For(block.Forms)
	while not input.IsEmpty:
		match TinyAstEvaluator(my(CompilerContext).Parameters).expansion(input):
			case SuccessfulMatch(Value: value, Input: input):
				yield value
