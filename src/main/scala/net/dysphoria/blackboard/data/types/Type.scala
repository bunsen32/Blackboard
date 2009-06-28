/*
 * Type.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data.types

abstract class Type
class Variable extends Type {
	var instance: Option[Type] = None
}
class NamedVariable(name: String) extends Variable {
	override def toString = name
}

case class Constr(name: String, args: Type*) extends Type

class InfixConstr(arg1: Type, name: String, arg2: Type) extends Constr(name, arg1, arg2) {
	override def toString = bracketIfInfix(arg1) +" "+ name +" "+ arg2

	private def bracketIfInfix(t: Type): String = t match {
		case _: InfixConstr => "("+ t.toString +")"
		case _ => t.toString
	}
}

class Function(arg: Type, res: Type) extends InfixConstr(arg, "→", res)

class Tuple(args: Type*) extends Constr("×", args: _*) {
	override def toString = args.mkString("(", ", ", ")")
}

class Monomorphic(name: String) extends Constr(name) {
	override def toString = name
}

object Boolean extends Monomorphic("Boolean")
object Int extends Monomorphic("Int")
object Real extends Monomorphic("Real")
object Complex extends Monomorphic("Complex")