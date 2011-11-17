namespace Boo.TinyAst

import System
import Boo.OMeta
import Boo.Adt
import Boo.OMeta.Parser

data Form = \
	Identifier(Test as string)

ometa TinyAstParser < BooParser:
	pass

