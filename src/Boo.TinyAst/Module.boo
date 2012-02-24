namespace Boo.TinyAst

import System
import System.Text
#import Boo.TinyAst.FormExtensions
import Boo.Lang.Compiler.Ast


class Module(Boo.Lang.Compiler.Ast.Module):
"""Description of Module"""
	public def constructor():
		pass
		
	public new def ToCodeString():
		sb = StringBuilder()

		if Namespace:
			sb.Append(Namespace.ToCodeString())
		
		if Imports.Count:
			for imp in Imports:
				sb.Append(imp.ToCodeString())

		for member in Members:
			sb.Append(member.ToCodeString())
				
		for global in Globals.Statements:
			if global isa MacroStatement and (global as MacroStatement)["tinyAst"]:
				form as Form = (global as MacroStatement)["tinyAst"]
				s = form.ToCodeString()
				sb.Append(s)
			else:
				sb.Append((global as MacroStatement).ToCodeString())
			sb.AppendLine()

//			foreach (Boo.Lang.Compiler.Ast.Attribute attribute in m.Attributes)
//				WriteModuleAttribute(attribute);
//
//			foreach (Boo.Lang.Compiler.Ast.Attribute attribute in m.AssemblyAttributes)
//				WriteAssemblyAttribute(attribute);



//		System.IO.StringWriter writer = new System.IO.StringWriter();
//		Accept(new Visitors.BooPrinterVisitor(writer));
//		return writer.ToString();
		
		return sb.ToString()

