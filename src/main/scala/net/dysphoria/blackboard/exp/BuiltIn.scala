/*
 * BuiltIn.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import blackboard.data.types._

object BuiltIn extends Env {

	val localTypeDefs = Map.empty ++ List(
		"Int" -> Int,
		"Boolean"-> Boolean,
		"Real" -> Real,
		"Rational"-> Rational,
		"Complex" -> Complex,
	)

	val localDefs = Map.empty ++ List(
		"+" -> new FunctionDefn("+", true, new Function(new Tuple(Int, Int), Int), Const((p:(Int, Int)) => p._1+p._2)),
		"*" -> new FunctionDefn("*", true, new Function(new Tuple(Int, Int), Int), Const((p:(Int, Int)) => p._1*p._2)),
		"-" -> new FunctionDefn("-", true, new Function(new Tuple(Int, Int), Int), Const((p:(Int, Int)) => p._1-p._2)),
		"/" -> new FunctionDefn("/", true, new Function(new Tuple(Int, Int), Int), Const((p:(Int, Int)) => p._1/p._2)),
		"&" -> new FunctionDefn("&", true, new Function(new Tuple(Int, Int), Int), Const((p:(Int, Int)) => p._1&p._2)),
		"|" -> new FunctionDefn("|", true, new Function(new Tuple(Int, Int), Int), Const((p:(Int, Int)) => p._1|p._2)),
	)

	def defs(key: String) =
		localDefs.getOrElse(key, NullEnv.defs(key))
	def typeDefs(key: String) =
		localTypeDefs.getOrElse(key, NullEnv.typeDefs(key))

}
