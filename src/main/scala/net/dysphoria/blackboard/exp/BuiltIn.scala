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

	val IntInt = new Tuple(Array(Int, Int))
	val FIntIntToInt = new Function(IntInt, Int)
	val FIntIntToBoolean = new Function(IntInt, Boolean)
	val StringString = new Tuple(Array(String, String))
	val FStringStringToString = new Function(StringString, String)
	val localDefs = Map.empty ++ List(
		"+" -> new FunctionDefn("+", true, FIntIntToInt, Const((p:(Int, Int)) => p._1+p._2, FIntIntToInt)),
		"*" -> new FunctionDefn("*", true, FIntIntToInt, Const((p:(Int, Int)) => p._1*p._2, FIntIntToInt)),
		"-" -> new FunctionDefn("-", true, FIntIntToInt, Const((p:(Int, Int)) => p._1-p._2, FIntIntToInt)),
		"/" -> new FunctionDefn("/", true, FIntIntToInt, Const((p:(Int, Int)) => p._1/p._2, FIntIntToInt)),
		"&" -> new FunctionDefn("&", true, FIntIntToInt, Const((p:(Int, Int)) => p._1&p._2, FIntIntToInt)),
		"|" -> new FunctionDefn("|", true, FIntIntToInt, Const((p:(Int, Int)) => p._1|p._2, FIntIntToInt)),
		"=" -> new FunctionDefn("=", true, FIntIntToBoolean, Const((p:(Int, Int)) => p._1==p._2, FIntIntToBoolean)),
		"append"-> new FunctionDefn("append", false, FStringStringToString, Const((p:(String, String))=> p._1+p._2, FStringStringToString)),
	)

	def defs(key: String) =
		localDefs.getOrElse(key, NullEnv.defs(key))
	def typeDefs(key: String) =
		localTypeDefs.getOrElse(key, NullEnv.typeDefs(key))

}
