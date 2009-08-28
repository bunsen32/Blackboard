/*
 * BuiltIn.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import blackboard.data.{types=>t}
import t.{core=>c}
import Ast._
import Annotations._

object BuiltIn {

	val typeList = List(
		TypeDef("Boolean", ResolvedType(c.Boolean)),
		TypeDef("Int", ResolvedType(c.Int)),
		TypeDef("Real", ResolvedType(c.Real)),
		TypeDef("Rational", ResolvedType(c.Rational)),
		TypeDef("Complex", ResolvedType(c.Complex)),
		TypeDef("String", ResolvedType(c.String))
	)
	val types = Map.empty ++ (for (d<-typeList) yield d.name -> d)

	
	val FBooleanToBoolean = new t.Function(c.Boolean, c.Boolean)

	val IntInt = new t.Tuple(Array(c.Int, c.Int))
	val FIntIntToInt = new t.Function(IntInt, c.Int)
	val FIntIntToBoolean = new t.Function(IntInt, c.Boolean)

	val RealReal = new t.Tuple(Array(c.Real, c.Real))
	val FRealToReal = new t.Function(c.Real, c.Real)
	val FRealRealToReal = new t.Function(RealReal, c.Real)
	val FRealRealToBoolean = new t.Function(RealReal, c.Boolean)

	val StringString = new t.Tuple(Array(c.String, c.String))
	val FStringStringToString = new t.Function(StringString, c.String)
	
	val functionList = List(
		/*nativeFunction("+", true, FIntIntToInt, p => p[Int](0) + p[Int](1)),
		nativeFunction("*", true, FIntIntToInt, p => p[Int](0) * p[Int](1)),
		nativeFunction("-", true, FIntIntToInt, p => p[Int](0) - p[Int](1)),
		nativeFunction("/", true, FIntIntToInt, p => p[Int](0) / p[Int](1)),
		nativeFunction("&", true, FIntIntToInt, p => p[Int](0) & p[Int](1)),
		nativeFunction("|", true, FIntIntToInt, p => p[Int](0) | p[Int](1)),
		nativeFunction("=", true, FIntIntToBoolean, p => p[Int](0) == p[Int](1)),*/
		nativeFunction("append", FStringStringToString, p=> p[String](0) + p[String](1)),

		nativeFunction("!", FBooleanToBoolean, p => !p[Boolean](0)),

		nativeFunction("inteq", FIntIntToBoolean, p => p[Int](0) == p[Int](1)),
		nativeFunction("intadd", FIntIntToInt, p => p[Int](0) + p[Int](1)),
		nativeFunction("intmult", FIntIntToInt, p => p[Int](0) * p[Int](1)),
		nativeFunction("intsub", FIntIntToInt, p => p[Int](0) - p[Int](1)),
		nativeFunction("intdiv", FIntIntToInt, p => p[Int](0) / p[Int](1)),
		nativeFunction("intmod", FIntIntToInt, p => p[Int](0) % p[Int](1)),

		nativeFunction("realeq", FRealRealToBoolean, p => p[Double](0) == p[Double](1)),
		nativeFunction("realadd", FRealRealToReal, p => p[Double](0) + p[Double](1)),
		nativeFunction("realmult", FRealRealToReal, p => p[Double](0) * p[Double](1)),
		nativeFunction("realsub", FRealRealToReal, p => p[Double](0) - p[Double](1)),
		nativeFunction("realdiv", FRealRealToReal, p => p[Double](0) / p[Double](1)),
		nativeFunction("sin", FRealToReal, p => Math.sin(p[Double](0))),
		nativeFunction("cos", FRealToReal, p => Math.cos(p[Double](0))),
		nativeFunction("tan", FRealToReal, p => Math.tan(p[Double](0)))
	)

	val functions = Map.empty ++ (for(f <- functionList) yield f.name -> f)

	def nativeFunction(name: String, typ: t.Function, op: NativeParams=>Any): ValueDef = {
		val fnValue = typ.arg match {
			case c.Unit => NativeFunction0(op)
			case many: t.Tuple => NativeFunctionN(op)
			case one => NativeFunction1(op)
		}
		VariableDef(name, ResolvedType(typ), Const(fnValue, typ)) where (Type -> typ)
	}
}

abstract class NativeParams {
	def apply[P](index: Int): P
}

abstract class NativeFunction {
	val op: NativeParams=>Any
}
case class NativeFunction0(override val op: NativeParams=>Any) extends NativeFunction
case class NativeFunction1(override val op: NativeParams=>Any) extends NativeFunction
case class NativeFunctionN(override val op: NativeParams=>Any) extends NativeFunction


