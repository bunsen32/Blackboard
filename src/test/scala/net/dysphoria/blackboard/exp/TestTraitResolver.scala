/*
 * TestTraitOrdering.scala
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

class TestTraitResolver extends FunSuite with ShouldMatchers {

	val PhasesBeforeTraitResolver =
		GrammarParser chain
		NameResolver chain
		EvaluationResolver

	def resolve(expr: String) = {
		import scala.util.parsing.input.CharSequenceReader
		val in = new CharSequenceReader(expr)
		PhasesBeforeTraitResolver.process(in, BuiltInEnv) match {
			case Left(failure) => error(failure.toString)
			case Right(exp) => TraitResolver.process(exp, BuiltInEnv)
		}
	}

	def testDisallowDuplicateParentTraits {
		resolve("{trait A T{}; trait B T extends A, A{}; true}") match {
			case Right(_) => error("Should not allow dupl parent traits")
			case Left(errors) =>
				errors.size should be (1)
				errors(0).getClass should be (classOf[ParseError])
		}
	}

	def testDisallowRecursiveTraits {
		resolve("{trait A T extends B{}; trait B T extends A{}; true}") match {
			case Right(_) => error("Should not allow recursive traits")
			case Left(errors) =>
				errors.size should be (1)
				errors(0).getClass should be (classOf[RecursiveTraitError])
		}
	}
}
