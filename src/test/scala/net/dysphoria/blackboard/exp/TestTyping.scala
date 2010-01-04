/*
 * testTyping.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import org.scalatest.FunSuite
import org.scalatest.matchers._

import net.dysphoria.blackboard._
import data.{types=>t}
import t.{core=>c}
import Annotations._

class TestTyping extends FunSuite with ShouldMatchers {

	val PhasesBeforeTyper =
		GrammarParser chain
		NameResolver chain
		EvaluationResolver chain
		TraitResolver


	def typeOf(expr: String) = {
		import scala.util.parsing.input.CharSequenceReader
		val in = new CharSequenceReader(expr)
		PhasesBeforeTyper.process(in, BuiltInEnv) match {
			case Left(errors) => error(errors.toString)
			case Right(untyped) =>
				Typer.process(untyped, BuiltInEnv) match {
					case Left(tErrors) => Left(tErrors(0))
					case Right(rt) => Right(rt*Type)
				}
		}
	}

	def testTypesAreMonomorphicWithinFunction {
		typeOf("function(f) = (f(3), f(true))") match {
			case Left(t: TypeMismatchError) => ;// what we expect
			case _ => error("Expected type mismatch!")
		}
	}

	def testTypesArePolymorphicInScope {
		typeOf("{function f(x) = x; (f(4), f(true))}") should be ('isRight)
	}

	def testNoRecursiveTypes {
		typeOf("function(x)=x(x)") match {
			case Left(t:RecursiveTypeError) => ; // expected
			case _ => error("Expected recursive type error!")
		}
	}

	def testConstantFunctionAppliedToItself {
		typeOf("{function g(f)=5; g(g)}") should be (Right(c.Int))
	}

	def testSomething {
		typeOf("function(g)={function f(x)=g; (f(3), f(true))}") should be ('isRight)
	}

	def testFunctionCompositionTuple {
		typeOf("function (f, g) = function(x) = g(f(x))") should be ('isRight)
	}

	def testFunctionCompositionCurry {
		typeOf("function (f) = function(g) = function(x) = g(f(x))") should be ('isRight)
	}

	def testFactorial {
		typeOf("{function fact(n) = if (inteq(n,0)) 1 else intmult(n, fact(intsub(n,1))); fact 5}") should be (Right(c.Int))
	}
}
