namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.TinyAst
import Boo.Lang.Environments
import Boo.Lang.Compiler

macro tinyAst:
	block = __macro["tinyAst"] as Block
	for form in block.Forms:
		match TinyAstEvaluator(my(CompilerContext).Parameters).stmt(OMetaInput.Singleton(form)):
			case SuccessfulMatch(Value: value):
				yield value