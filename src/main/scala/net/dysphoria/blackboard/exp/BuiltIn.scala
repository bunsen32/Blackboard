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
		"+" -> nativeFunction("+", true, FIntIntToInt, p => p[Int](0)+p[Int](1)),
		"*" -> nativeFunction("*", true, FIntIntToInt, p => p[Int](0)*p[Int](1)),
		"-" -> nativeFunction("-", true, FIntIntToInt, p => p[Int](0)-p[Int](1)),
		"/" -> nativeFunction("/", true, FIntIntToInt, p => p[Int](0)/p[Int](1)),
		"&" -> nativeFunction("&", true, FIntIntToInt, p => p[Int](0)&p[Int](1)),
		"|" -> nativeFunction("|", true, FIntIntToInt, p => p[Int](0)|p[Int](1)),
		"=" -> nativeFunction("=", true, FIntIntToBoolean, p => p[Int](0)==p[Int](1)),
		"append"-> nativeFunction("append", false, FStringStringToString, p=> p[String](0)+p[String](1)),
	)

	def defs(key: String) =
		localDefs.getOrElse(key, NullEnv.defs(key))
	def typeDefs(key: String) =
		localTypeDefs.getOrElse(key, NullEnv.typeDefs(key))

	def nativeFunction(name: String, infix: Boolean, typ: Function, op: NativeParams=>Any) = {
		assume(typ.arg.asInstanceOf[Tuple].args.length > 1)
		new FunctionDefn(name, infix, typ, Const(NativeFunctionN(op), typ))
	}
}
