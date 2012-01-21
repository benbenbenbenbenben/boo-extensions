namespace Boo.TinyAst.Macro

import System
import Boo.Lang.Compiler.Ast
import Boo.Lang.PatternMatching

macro afterMacroExpansion:
	for stmt in afterMacroExpansion.Body.Statements:
		match stmt:
			case YieldStatement(Expression: QuasiquoteExpression(Node: node)):
				yield node

