namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.Lang.Compiler.Ast
import Boo.TinyAst

macro tinyAst:
	match TinyAstEvaluator().ast(OMetaInput.Singleton(__macro["tinyAst"])):
		case SuccessfulMatch(Value: value):
			yield value