namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.TinyAst
import Boo.Lang.Environments
import Boo.Lang.Compiler

macro tinyAst:
	form = __macro["tinyAst"] as Form

	match TinyAstEvaluator(my(CompilerContext).Parameters).expansion(OMetaInput.Singleton(form)):
		case SuccessfulMatch(Value: value):
			pass
	yield value	
