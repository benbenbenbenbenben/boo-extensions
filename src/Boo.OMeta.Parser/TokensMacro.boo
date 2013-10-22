namespace Boo.OMeta.Parser

import Compiler.Ast
import Boo.Lang.PatternMatching
import Boo.OMeta

macro tokens:
"""
Generates token rules. The generated code relies on the existence of a
parameterized token rule.

From:
	
	tokens:
		eq = "="
		id = ++letters
	
it generates:

	eq = "=" >> value ^ makeToken("eq", value)
	id = ++letters >> value ^ makeToken("id", value)
	tokens = eq | id
	EQ = token["eq"]
	ID = token["id"]
"""
	block as Block = tokens.ParentNode

	rules = []
	for stmt in tokens.Body.Statements:
		match stmt:
			case ExpressionStatement(Expression: [| $name = $pattern |]):
				e = [| $name = $pattern >> value ^ makeToken($(name.ToString()), value) |]
				e.LexicalInfo = stmt.LexicalInfo
				block.Add(e)
				
				tokenRule = [| $(ReferenceExpression(Name: name.ToString().ToUpper())) = token[$(name.ToString())] |]
				e.LexicalInfo = stmt.LexicalInfo
				block.Add(tokenRule)
				rules.Add(name)
	
	block.Add([| tokens = $(choicesRuleFrom(rules)) |])
