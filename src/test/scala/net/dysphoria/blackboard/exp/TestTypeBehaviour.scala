/*
 * testTyping.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import org.scalatest.FunSuite
import org.scalatest.matchers._

import data.{types=>t}
import t.{core=>c}
import Annotations._

class TestTypeBehaviour extends FunSuite with ShouldMatchers {

	def testChain {
		val A = new t.Monomorphic("A")
		val B = new t.Monomorphic("B")
		val C = new t.Monomorphic("C")
		val beh = (t.Behaviour.empty
			.withSubtype(A, B)
			.withSubtype(B, C))

		assert(beh.isSubtypeOf(A, B))
		assert(beh.isSubtypeOf(B, C))
		assert(beh.isSubtypeOf(A, C))

		assert(!beh.isSubtypeOf(B, A))
		assert(!beh.isSubtypeOf(C, B))
		assert(!beh.isSubtypeOf(C, A))
	}

	def testChain4 {
		val A = new t.Monomorphic("A")
		val B = new t.Monomorphic("B")
		val C = new t.Monomorphic("C")
		val D = new t.Monomorphic("D")
		val beh = (t.Behaviour.empty
			.withSubtype(A, B)
			.withSubtype(C, D)
			.withSubtype(B, C))

		assert(beh.isSubtypeOf(A, B))
		assert(beh.isSubtypeOf(B, C))
		assert(beh.isSubtypeOf(C, D))
		assert(beh.isSubtypeOf(A, C))
		assert(beh.isSubtypeOf(A, D))
		assert(beh.isSubtypeOf(B, D))

		assert(!beh.isSubtypeOf(B, A))
		assert(!beh.isSubtypeOf(C, B))
		assert(!beh.isSubtypeOf(D, C))
		assert(!beh.isSubtypeOf(C, A))
		assert(!beh.isSubtypeOf(D, A))
		assert(!beh.isSubtypeOf(D, B))
	}

	def testFan {
		val A = new t.Monomorphic("A")
		val B = new t.Monomorphic("B")
		val C = new t.Monomorphic("C")
		val D = new t.Monomorphic("D")
		val E = new t.Monomorphic("E")
		val F = new t.Monomorphic("F")
		val beh0 = (t.Behaviour.empty
			.withSubtype(A, C)
			.withSubtype(B, C)
			.withSubtype(D, E)
			.withSubtype(D, F))
		val beh1 = beh0.withSubtype(C, D)

		assert(!beh0.isSubtypeOf(A, E))
		assert( beh1.isSubtypeOf(A, E))

		assert(!beh0.isSubtypeOf(A, F))
		assert( beh1.isSubtypeOf(A, F))

		assert(!beh0.isSubtypeOf(A, D))
		assert( beh1.isSubtypeOf(A, D))

		assert(!beh0.isSubtypeOf(B, E))
		assert( beh1.isSubtypeOf(B, E))

		// Double-check that the reverse relation still does not work.
		assert(!beh0.isSubtypeOf(E, B))
		assert(!beh1.isSubtypeOf(E, B))
	}


	def testDisallowCycles {
		val A = new t.Monomorphic("A")
		val B = new t.Monomorphic("B")
		val C = new t.Monomorphic("C")
		intercept[Exception]{
			val beh = (t.Behaviour.empty
				.withSubtype(A, B)
				.withSubtype(B, C)
				.withSubtype(C, A))
			()
		}
	}
}
