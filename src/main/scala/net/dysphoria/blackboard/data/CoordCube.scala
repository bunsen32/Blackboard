/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.data

class CoordCube(val ranges: Seq[Range]) {
	def arity = ranges.size
	def apply(d: Int) = ranges(d)
	def update(d: Int, newR: Range) = {
		val result = ranges.toArray
		result(d) = newR
		new CoordCube(result)
	}
	def contains(c: Coord) = (ranges equalsWith c)(_ contains _)
	def volume = (1 /: ranges)(_ * _.length)
	def isEmpty = ranges exists (_.size == 0)
	
	def contains(containee: CoordCube) =
		(this.ranges equalsWith containee.ranges)(numberRangeContains(_, _))
	private def numberRangeContains(container: Range, containee: Range) =
		(container.start <= containee.start) && (container.end >= containee.end)

	def intersects(other: CoordCube) =
		(this.ranges equalsWith other.ranges)(numberRangeIntersects(_, _))
	private def numberRangeIntersects(r1: Range, r2: Range) =
		(r1.start < r2.end) && (r1.end > r2.start)

	override def toString = {
		ranges map (d => d.start+"-"+d.last) mkString("(", ", ", ")")
	}
}

object CoordCube {
	implicit def seq2cube(s: Seq[Range]) = new CoordCube(s)
	implicit def coordToCube(coord: Coord): CoordCube =
		coord map (v => new Range(v, v+1, 1))
}