namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.TinyAst

macro tinyAst:
	block = __macro["tinyAst"] as Block
	for form in block.Forms:
		match TinyAstEvaluator().stmt(OMetaInput.Singleton(form)):
			case SuccessfulMatch(Value: value):
				yield value