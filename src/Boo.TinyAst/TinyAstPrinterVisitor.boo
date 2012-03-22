namespace Boo.TinyAst

import System
import Boo.Lang.PatternMatching
import Boo.Lang.Compiler.Ast
import Boo.TinyAst

macro afterMacroExpansion:
	for stmt in afterMacroExpansion.Body.Statements:
		match stmt:
			case YieldStatement(Expression: QuasiquoteExpression(Node: node)):
				yield node

afterMacroExpansion:
	yield [|
		class FormDepthFirstVisitor(IFormVisitor):	
			
			virtual def OnBlock(node as Block):
				if node.Forms:
					for item in node.Forms:
						item.Accept(self)
			
			virtual def OnPair(node as Pair):
				node.Left.Accept(self) if node.Left
				node.Right.Accept(self) if node.Right
			
			virtual def OnTuple(node as Tuple):
				if node.Forms:
					for item in node.Forms:
						item.Accept(self)				

			virtual def OnPrefix(node as Prefix):
				if node.IsPostfix:
					node.Operand.Accept(self)
					node.Operator.Accept(self)
				else:
					node.Operator.Accept(self)
					node.Operand.Accept(self)					

			virtual def OnInfix(node as Infix):
				node.Left.Accept(self)
				node.Right.Accept(self)

			virtual def OnLiteral(node as Literal):
				pass

			virtual def OnBrackets(node as Brackets):
				node.Form.Accept(self) if node.Form

			virtual def OnIdentifier(node as Identifier):
				pass
	|]		
	
	yield [|		
		class TinyAstPrinterVisitor(FormDepthFirstVisitor):
			protected _indent = 0
			protected _indentText = '\t'
			
			private _writer as IO.StringWriter
			
			def constructor(writer as IO.StringWriter):
				_writer = writer				
				
			override def OnBlock(node as Block):
				startFromNewLine = false
				if node.Forms:
					for item in node.Forms:
						WriteLine() if startFromNewLine
						item.Accept(self)
						startFromNewLine = true
						

			override def OnPair(node as Pair):
				node.Left.Accept(self) if node.Left
				Write(":")
				if node.IsMultiline:
					Indent()
					WriteLine()
				else:
					Write(" ")					
				
				node.Right.Accept(self) if node.Right
				
				if node.IsMultiline:
					Dedent()

			override def OnTuple(node as Tuple):
				commaNeeded = false
				
				if node.Forms:
					for item in node.Forms:
						if commaNeeded:
							Write(", ")
						Write("(") if item isa Infix	
						item.Accept(self)
						Write(")") if item isa Infix
						
						commaNeeded = true
				
				Write(",") if len(node.Forms) == 1 

			override def OnPrefix(node as Prefix):
				if node.IsPostfix:
					node.Operand.Accept(self)
					Write(" ")
					node.Operator.Accept(self)
				else:
					Write("(") if node.Operator isa Infix
					node.Operator.Accept(self)
					Write(")") if node.Operator isa Infix
					
					if (not node.Operand isa Brackets) or (node.Operator isa Identifier and (node.Operator as Identifier).IsKeyword):
						Write(" ") if not (node.Operator isa Identifier and (node.Operator as Identifier).IsSymbol)
					Write("(") if node.Operand isa Infix
					node.Operand.Accept(self)
					Write(")") if node.Operand isa Infix

			override def OnInfix(node as Infix):
				
				enclose = node.Left isa Infix
				Write("(") if enclose
				node.Left.Accept(self)
				Write(")") if enclose
				
				#Write(" $(node.Operator) ")
				Write(" ")
				node.Operator.Accept(self)
				Write(" ")
				
				enclose = node.Right isa Infix
				Write("(") if enclose
				node.Right.Accept(self)
				Write(")") if enclose
			
			override def OnLiteral(node as Literal):				
				Write("'") if node.Value isa string
				if node.Value isa double:
					Write((node.Value cast double).ToString("########0.0##########", System.Globalization.CultureInfo.InvariantCulture))
				elif node.Value isa long:
					Write(node.Value.ToString())
					Write("L") if node.Value cast long > int.MaxValue or node.Value cast long < int.MinValue
				else:
					Write(node.Value.ToString())
				Write("'") if node.Value isa string
				
			override def OnBrackets(node as Brackets):
				Write(GetStartBracket(node.Type))
				if node.Form isa Block:
					Indent()
					WriteLine()
				
				Write("(") if node.Form isa Infix				

				node.Form.Accept(self) if node.Form

				Write(")") if node.Form isa Infix

				if node.Form isa Block:
					Dedent()
					WriteLine()
				Write(GetEndBracket(node.Type))

			override def OnIdentifier(node as Identifier):
				Write(node.Name)				
				
			virtual def WriteLine():
				_writer.WriteLine()
				WriteIndented()
				
			virtual def Write(s as string):
				_writer.Write(s)
				
			virtual def Indent():
				_indent++
				
			virtual def Dedent():
				_indent--
				
			virtual def WriteIndented():
				i = 0
				while i++ < _indent:
					_writer.Write(_indentText)
				
			private def GetStartBracket(kind as BracketsType):
				match kind:
					case BracketsType.QQ:
						return "[|"
					case BracketsType.Parenthesis:
						return "("
					case BracketsType.Curly:
						return "{"
					case BracketsType.Square:
						return "["
						
			private def GetEndBracket(kind as BracketsType):
				match kind:
					case BracketsType.QQ:
						return "|]"
					case BracketsType.Parenthesis:
						return ")"
					case BracketsType.Curly:
						return "}"
					case BracketsType.Square:
						return "]"

	|]