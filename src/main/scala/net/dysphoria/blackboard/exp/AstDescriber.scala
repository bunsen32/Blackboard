/*
 * AstDescriber.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

object AstDescriber {
	import Ast._
	import Annotations._

	def describe(exp: Node): String = {
		val s = describe1(exp)
		exp.get(ResolvedRef) match {
			case Some(id) => s+"*"+id
			case _ => s
		}
	}

	def describe1(exp: Node): String = exp match {
		// EXPRESSIONS
		case If(pred, a, b) => "if "+describeBracketed(pred)+" "+describe(a)+" else "+describe(b)
		//case Match(param, cases@_*) => describe(param)+" match {}"
		case Block(result, defs, typedefs, tbehaviours) => defs.mkString("{\n",";\n", ";\n"+describe(result)+"}")
		case Module(defs, typedefs, tbehaviours) => defs.mkString("{\n",";\n", "\n}")
		case Lambda(body, params, typeparams) => "function("+params.mkString(",")+") = "+describe(body)
		case Tuple(terms) => terms.mkString("(", ", ", ")")
		case Const(a:String, t) => '"' + a + '"'
		case Const(a, t) => a.toString
		case Apply(fn, args) => describe(fn)+describeBracketed(args)
		case ValueRef(ident) => ident
		case Evaluation(terms, tail) => terms.mkString(" ") + (tail match {
			case None => ""
			case Some(TernaryTail(x, y)) => " ? "+x.toString+" : "+y.toString
		})

		// DEFINITIONS
		case VariableDef(name, typ, value) => "var "+name+typeAnnot(typ)+" = "+describe(value)
		case FunctionDef(name, op, impl, typ, value) => (if(impl)"implicit "else"")+"function "+name+"("+value.params.mkString(", ")+") = "+describe(value.body)
		case AbstractFunctionDef(name, op, impl, typ)=> (if(impl)"implicit "else"")+"abstract function " + name + typeAnnot(typ)
		case Param(name, typ) => name+typeAnnot(typ)

		// TYPE EXPRESSIONS
		case _:NoTypeExpClass => "<no type>"
		case _:UnitTypeExpClass => "()"
		case FunctionTypeExp(arg, res) => arg+"->"+res
		case TupleTypeExp(els) => els.mkString("(", ", ", ")")
		case TypeLikeRef(name, args) => name+(if(args.isEmpty) "" else args.mkString(","))
		case ResolvedType(typ) => "["+typ+"]"

		case e => e.getClass.toString
	}
	
	def describeBracketed(exp: Node): String = {
		val result = describe(exp)
		if (result(0)=='(') result else "("+result+")"
	}

	def typeAnnot(typeExp: TypeExp): String = typeExp match {
		case _: NoTypeExpClass => ""
		case t => ": " + describe(t)
	}

}

