/*
 * testTyping.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import org.scalatest.FunSuite
import org.scalatest.matchers._

import blackboard.data.types._

class TestTyping extends FunSuite with ShouldMatchers {

	def typeOf(expr: String) = {
		import scala.util.parsing.input.CharSequenceReader
		val in = new CharSequenceReader(expr)
		GrammarParser.parseExpression(in) match {
			case GrammarParser.Success(exp, _) => {
				val resolved = NameResolver.resolve(exp)(BuiltIn)
				Typer.analyse(resolved)
			}
			case failure => error(failure.toString)
		}
	}

	def testTypesAreMonomorphicWithinFunction {
		intercept[exp.TypeMismatchException]{
			typeOf("function(f) = (f(3), f(true))")
		}
	}

	def testTypesArePolymorphicInScope {
		typeOf("{function f(x) = x; (f(4), f(true))}")
	}

	def testNoRecursiveTypes {
		intercept[RecursiveTypeException]{
			typeOf("function(x)=x(x)")
		}
	}

	def testConstantFunctionAppliedToItself {
		typeOf("{function g(f)=5; g(g)}") should be (Int)
	}

	def testSomething {
		typeOf("function(g)={function f(x)=g; (f(3), f(true))}")
	}

	def testFunctionCompositionTuple {
		typeOf("function (f, g) = function(x) = g(f(x))")
	}

	def testFunctionCompositionCurry {
		typeOf("function (f) = function(g) = function(x) = g(f(x))")
	}

	def testFactorial {
		typeOf("{function fact(n) = if (n=0) 1 else n * fact(n-1); fact 5}") should be (Int)
	}
}
