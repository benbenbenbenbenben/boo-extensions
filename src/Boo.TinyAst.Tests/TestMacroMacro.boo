namespace Boo.Lang.Extensions

import System
import Boo.Lang.PatternMatching
import Boo.OMeta
import Boo.Lang.Compiler.Ast
import Boo.TinyAst.Tests


macro testMacro:
	match ExternalParser().exp(OMetaInput.Singleton(__macro["someData"])):
		case SuccessfulMatch(Value: value = BinaryExpression()):
			yield [|print $(value)|]


