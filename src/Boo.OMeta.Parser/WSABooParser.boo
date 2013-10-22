namespace Boo.OMeta.Parser
	
import Boo.OMeta
import Boo.Lang.Compiler.Ast
import Boo.Lang.Compiler
	
ometa WSABooParser(compilerParameters as CompilerParameters) < BooParser(compilerParameters):
	scanner = (empty_lines ^ makeToken("eol")) | ((--space, tokens >> t) ^ t)

	keywords = ~"pass", ("end" | super)
	
	begin_block = COLON, eol
	end_block = (keyword["end"], eol) | (~~ELSE) | (~~ELIF) | (~~(OR, COLON)) | (~~THEN) | (~~EXCEPT) | (~~ENSURE)
	empty_block = (begin_block, end_block) ^ Block()
	
	member_reference = (((member_reference >> e,  DOT, enterWhitespaceAgnosticRegion, ID >> name, leaveWhitespaceAgnosticRegion) \
		^ newMemberReference(e, tokenValue(name))) | slicing) >> e, (INCREMENT | DECREMENT | "") >> postOp ^ addSuffixUnaryOperator(e, postOp)
	
	INDENT = eol | ""
	DEDENT = eol | ""
	
	class_body = (--class_member >> members ^ members)
	struct_body = (--struct_member >> members ^ members)
	interface_body = (--interface_member >> members ^ members)

