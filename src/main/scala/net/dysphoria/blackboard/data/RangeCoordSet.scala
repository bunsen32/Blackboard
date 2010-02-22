/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.data

import scala.collection.{mutable, immutable}

/*
 * Relatively efficient representation of sets of points where the points are
 * mostly contained within contiguous (hyper)cubes (e.g., from selecting a range
 * of spreadsheet cells...). Points are represented as Seq[Int]
 */
class RangeCoordSet(val ranges: immutable.Set[CoordCube]) extends immutable.Set[Coord] {
	def this(initialRange: CoordCube) = this(Set(initialRange))

	override def contains(coords: Coord) = ranges exists (_ contains coords)
	override def isEmpty = ranges forall (_.isEmpty)

	override def elements = throw new UnsupportedOperationException
	override def size = throw new UnsupportedOperationException(
		"We currently store duplicate overlapping points. Cannot return # of points")

	override def empty[B] = Set.empty

	// We currently allow overlapping ranges
	def numberOfCellsIncludingDuplicates = (0 /: ranges)(_ + _.volume)

	override def ++(other: Iterable[Coord]) = other match {
		case other: RangeCoordSet => (this /: other.ranges)(_ + _)
		case _ => super.++(other)
	}

	override def +(coords: Coord): RangeCoordSet = this + (coords:CoordCube)

	def +(newRange: CoordCube) = {
		if (newRange.isEmpty || (ranges exists (_ contains newRange)))
			this

		else{
			val withoutSwallowedRanges = ranges filter (! newRange.contains(_))
			new RangeCoordSet(withoutSwallowedRanges + newRange)
		}
	}


	override def --(other: Iterable[Coord]) = other match {
		case other: RangeCoordSet => (this /: other.ranges)(_ - _)
		case _ => super.--(other)
	}

	override def -(coords: Coord): RangeCoordSet = this - (coords:CoordCube)

	def -(exclRange: CoordCube) = {
		if (exclRange.isEmpty || !(ranges exists (_ intersects exclRange)))
			this

		else {
			var choppedUp = ranges flatMap (r => {
				if (exclRange contains r)
					Set.empty[CoordCube]

				else if (exclRange intersects r)
					chop(r, exclRange, 0)

				else
					Set(r)
			})
			new RangeCoordSet(choppedUp)
		}
	}

	private def chop(r: CoordCube, excl: CoordCube, dix: Int): Iterable[CoordCube] = {
		require(r.arity == excl.arity)
		val arity = r.arity
		var result:List[CoordCube] = Nil
		val d = r(dix)
		val dexcl = excl(dix)
		assert(numberRangeIntersects(d, dexcl))
		if (d.start < dexcl.start) {
			result ::= r.update(dix, new Range(d.start, dexcl.start, 1))
		}
		if (dix < arity - 1){
			val d1 = d.start max dexcl.start
			val d2 = d.end min dexcl.end
			val mid = r.update(dix, new Range(d1, d2, 1))
			result ++= chop(mid, excl, dix+1)
		}
		if (dexcl.end < d.end){
			result ::= r.update(dix, new Range(dexcl.end, d.end, 1))
		}
		result
	}

	private def numberRangeIntersects(r1: Range, r2: Range) =
		(r1.start < r2.end) && (r1.end > r2.start)

	override def toString = ranges.mkString("RangeCoordSet(", ",", ")")

}

object RangeCoordSet {

	val empty = new RangeCoordSet(Set.empty[CoordCube])

	def apply(v: Coord): RangeCoordSet = apply(v: CoordCube)
	implicit def apply(vs: CoordCube): RangeCoordSet = new RangeCoordSet(Set(vs))
	implicit def apply(vs: Set[Coord]) = empty ++ vs
}


