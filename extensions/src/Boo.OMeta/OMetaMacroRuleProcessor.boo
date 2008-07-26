namespace Boo.OMeta

import Boo.Lang.Compiler.Ast
import Boo.PatternMatching

class OMetaMacroRuleProcessor:
	
	_ruleName as string
	_collectingParseTree as DynamicVariable[of bool]

	def constructor(ruleName as string, options as List):
		_ruleName = ruleName
		_collectingParseTree = DynamicVariable[of bool]("ParseTree" in options)
	
	def expand(e as Expression, args as (Expression)) as Block:
		
		input = [| input_ |]
		block = Block(LexicalInfo: e.LexicalInfo)
		for arg in args:
			code = [|
				block:
					lastMatch = any($input)
					smatch = lastMatch as SuccessfulMatch
					if smatch is null: return lastMatch
					$input = smatch.Input
					$arg = smatch.Value
			|].Block
			block.Add(code)
		
		expand block, e, input, [| lastMatch |]
		block.Add([| return lastMatch |])
		
		return block
		
	def expand(e as Expression, input as Expression, lastMatch as ReferenceExpression) as Block:
		block = Block()
		expand block, e, input, lastMatch
		return block
		
	def expandChoices(block as Block, choices as List, input as Expression, lastMatch as ReferenceExpression):
		temp = uniqueName()
		
		currentBlock = block
		for choice in choices:
			expand currentBlock, choice, input, temp
			code = [|
				if $temp isa SuccessfulMatch:
					$lastMatch = $temp
				else:
					pass
			|]
			currentBlock.Add(code)
			currentBlock = code.FalseBlock
		currentBlock.Add([| $lastMatch = FailedMatch($input) |])
		
	def resultAppend(result as Expression):
		if collectingParseTree:
			return [| $result.Add(smatch.Value) if smatch.Value is not null |]
		return ExpressionStatement([| $result = smatch.Value |])
		
	def expandRepetition(block as Block, e as Expression, input as Expression, lastMatch as ReferenceExpression):
		
		temp = lastMatch #uniqueName()
		result = uniqueName()
		block.Add(expand(e, input, temp))
		block.Add([| smatch = $temp as SuccessfulMatch |])
		code = [|
			if smatch is not null:
				$(resultAppend(result))
				$(expandRepetitionLoop(e, [| $temp.Input |], temp, result)) 
		|]
		if collectingParseTree:
			code.TrueBlock.Insert(0, [| $result = [] |])
		block.Add(code)	
		
	def expandRepetitionLoop(e as Expression, input as Expression, lastMatch as ReferenceExpression, result as Expression):
		tempInput = uniqueName()
		code = [|
			block:
				$tempInput = $input
				while true:
					$(expand(e, [| $tempInput |], lastMatch))
					smatch = $lastMatch as SuccessfulMatch
					break if smatch is null
					$tempInput = smatch.Input
					$(resultAppend(result))

				$lastMatch = SuccessfulMatch($lastMatch.Input, $result)
		|]
		return code.Block
		
	def collectChoices(choices as List, e as Expression):
		match e:
			case [| $l | $r |]:
				collectChoices choices, l
				collectChoices choices, r
			otherwise:
				choices.Add(e)
				
	collectingParseTree:
		get: return _collectingParseTree.Value
				
	def expandSequence(block as Block, sequence as ExpressionCollection, input as Expression, lastMatch as ReferenceExpression):
		
		if collectingParseTree:
			expandSequenceWithParseTree block, sequence, input, lastMatch
		else:
			expandSequenceWithoutParseTree block, sequence, input, lastMatch
		
	def expandSequenceWithoutParseTree(block as Block, sequence as ExpressionCollection, input as Expression, lastMatch as ReferenceExpression):
		
		currentBlock = block
		for item in sequence.ToArray()[:-1]:
			expand currentBlock, item, input, lastMatch
			input = [| $lastMatch.Input |]
			code = [|
				if $lastMatch isa SuccessfulMatch:
					pass
			|]
			currentBlock.Add(code)
			currentBlock = code.TrueBlock
		expand currentBlock, sequence[-1], input, lastMatch
		
	def expandSequenceWithParseTree(block as Block, sequence as ExpressionCollection, input as Expression, lastMatch as ReferenceExpression):
		
		result = uniqueName()
		currentBlock = block
		currentBlock.Add([| $result = [] |])
		
		for item in sequence:
			expand currentBlock, item, input, lastMatch
			currentBlock.Add([| smatch = $lastMatch as SuccessfulMatch |])
			code = [|
				if smatch is not null:
					$(resultAppend(result))
			|]
			currentBlock.Add(code)
			currentBlock = code.TrueBlock
			input = [| $lastMatch.Input |]
			
		currentBlock.Add([| $lastMatch = SuccessfulMatch(smatch.Input, $result) |])
		
	def expandNegation(block as Block, rule as Expression, input as Expression, lastMatch as ReferenceExpression):
		oldInput = uniqueName()
		block.Add([| $oldInput = $input |])
		
		_collectingParseTree.With(false):
			expand block, rule, input, lastMatch
		block.Add([| smatch = $lastMatch as SuccessfulMatch |])
		code = [|
			if smatch is null:
				$lastMatch = SuccessfulMatch($oldInput, null)
			else:
				$lastMatch = FailedMatch($oldInput)
		|]
		block.Add(code)
		return code
		
	def expand(block as Block, e as Expression, input as Expression, lastMatch as ReferenceExpression):
		match e:
			case SpliceExpression(Expression: rule):
				block.Add([| input = $input |])
				block.Add([| $lastMatch = $rule |])
				
			case [| $rule[$arg] |]:
				newInput = uniqueName()
				block.Add([| $newInput = OMetaInput.Prepend($arg, $input) |])
				expand block, rule, newInput, lastMatch
				
			case [| $pattern and $predicate |]:
				expand block, pattern, input, lastMatch
				checkPredicate = [|
					if $lastMatch isa SuccessfulMatch and not $predicate:
						$lastMatch = FailedMatch($input)
				|]
				block.Add(checkPredicate)
				
			case [| $pattern ^ $value |]:
				_collectingParseTree.With(false):
					expand block, pattern, input, lastMatch
					code = [|
						block:
							smatch = $lastMatch as SuccessfulMatch
							if smatch is not null:
								$lastMatch = SuccessfulMatch(smatch.Input, $value)
					|].Block
					block.Add(code)
				
			case [| $pattern >> $variable |]:
				_collectingParseTree.With(true):
					expand block, pattern, input, lastMatch
					code = [|
						block:
							smatch = $lastMatch as SuccessfulMatch
							if smatch is not null:
								$variable = smatch.Value
					|].Block
					block.Add(code)
				
			case [| $_ | $_ |]:
				choices = []
				collectChoices choices, e
				expandChoices block, choices, input, lastMatch
				
			case StringLiteralExpression():
				block.Add([| $lastMatch = characters($input, $e) |])
				
			case [| ++$rule |]:
				expandRepetition block, rule, input, lastMatch
				
			case [| --$rule |]:
				result = uniqueName()
				block.Add([| $result = [] |]) if collectingParseTree
				block.Add(expandRepetitionLoop(rule, input, lastMatch, result))
				
			case [| ~$rule |]:
				expandNegation block, rule, input, lastMatch
				
			case ReferenceExpression(Name: name):
				block.Add([| $lastMatch = context.Apply(context, $name, $input) |])
				
			case [| super |]:
				block.Add([| $lastMatch = context.SuperApply(context, $_ruleName, $input) |])
				
			case [| $_() |]:
				rules = processObjectPatternRules(e)
				condition = PatternExpander().expand([| smatch.Value |], e)
				code = [|
					block:
						$lastMatch = any($input)
						smatch = $lastMatch as SuccessfulMatch
						if smatch is not null:
							if $condition:
								$(expandObjectPatternRules(rules, lastMatch))
							else:
								$lastMatch = FailedMatch($input)
				|].Block
				block.Add(code) 
				
			case ArrayLiteralExpression(Items: items):
				match items[0]:
					case [| ~$rule |]:
						negation = expandNegation(block, rule, input, lastMatch)
						if len(items) > 2:
							expandSequence negation.TrueBlock, items.PopRange(1), input, lastMatch
						else:
							expand negation.TrueBlock, items[1], input, lastMatch
					otherwise:
						expandSequence block, items, input, lastMatch 
				
	def processObjectPatternRules(pattern as Expression):
		rules = []
		processObjectPatternRules rules, pattern
		return rules
		
	def expandObjectPatternRules(rules, lastMatch as Expression) as Block:
		block = Block()
		input = uniqueName()
		
		currentBlock = block
		for temp as Expression, rule as Expression in rules:
			block.Add([| $input = OMetaInput.Singleton($temp) |])
			expand block, rule, input, lastMatch
			code = [|
				if $lastMatch isa SuccessfulMatch:
					pass
			|]
			currentBlock.Add(code)
			currentBlock = code.TrueBlock
		return block
		
	def processObjectPatternRules(rules as List, pattern as MethodInvocationExpression):
		for arg in pattern.NamedArguments:
			match arg.Second:
				case [| $_ >> $_ |]:
					temp = uniqueName()
					rules.Add((temp, arg.Second))
					arg.Second = temp
				otherwise:
					pass
			