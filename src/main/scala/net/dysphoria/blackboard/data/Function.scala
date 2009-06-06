/*
 * Function.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data

trait Function {
	def arity: Int
	def apply(in: Seq[Any]): Any
}

trait Function0 extends Function {
	final override def arity = 0
	override def apply(args: Seq[Any]) = {
		require(args.length == 0)
		apply0()
	}
	def apply0(): Any
}

trait Function1 extends Function {
	final override def arity = 1
	override def apply(args: Seq[Any]) = {
		require(args.length == 1)
		apply1(args(0))
	}
	def apply1(arg0: Any): Any
}


trait Bijection extends Function {
   def inverse: Bijection
}

trait Bijection1 extends Function1 with Bijection {
	override def inverse: Bijection1
}