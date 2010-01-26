/*
 * Coord.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data

class Coord(s: Seq[Int]) extends Seq[Int] {
	def arity = s.size
	def apply(d: Int) = s(d)
	def update(d: Int, x: Int) = {
		val result = s.toArray
		result(d) = x
		new Coord(result)
	}
	def to(other: Coord): CoordCube = {
		require(this.arity == other.arity)
		val result = new Array[Range](arity)
		for(d <- 0 until arity){
			val a = this(d)
			val b = other(d)
			result(d) = new Range(a min b, (a max b)+1, 1)
		}
		new CoordCube(result)
	}

	override def length = arity
	override def elements = s.elements
	def iterator = elements

	override def toString = mkString("(", ", ", ")")
	override def equals(other: Any) = other match {
		case b: Coord => (this equalsWith b)(_ == _)
		case _ => false
	}
	override def hashCode = (41 /: s)(_ + 41 * _)
}

object Coord {
	implicit def seq2coord(s: Seq[Int]) = new Coord(s)
}

