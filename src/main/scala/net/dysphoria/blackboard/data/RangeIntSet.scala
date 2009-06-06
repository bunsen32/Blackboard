/*
 * RangeIntSet.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data

/**
 * 'Efficient' implementation of a set of integers; efficient in that contiguous
 * ranges of integers are encoded efficiently. Not efficient for distributed
 * ints or for large sets.
 */
class RangeIntSet(val ranges: Set[Range]) extends Set[Int] {
	override def contains(v: Int) = ranges exists (_ contains v)
	override def elements = ((Iterator.empty:Iterator[Int]) /: ranges)(_ ++ _.elements)

	override def size = (0 /: ranges.elements)(_ + _.size)


	def empty[B] = Set.empty

	override def ++(other: Iterable[Int]) = other match {
		case other: RangeIntSet => (this /: other.ranges)(_ + _)
		case _ => super.++(other)
	}

	override def +(v: Int) = this + (v to v)

	def +(newRange: Range) = 
		if (newRange.isEmpty || (ranges exists (rangeContains(_, newRange))))
			this

		else{
			val withoutSwallowedRanges = ranges filter (!rangeContains(newRange, _))
			new RangeIntSet(excludeRange(ranges, newRange) + newRange)
		}


	override def --(other: Iterable[Int]) = other match {
		case other: RangeIntSet => (this /: other.ranges)(_ - _)
		case _ => super.--(other)
	}

	override def -(v: Int) = this - (v to v)

	def -(exclRange: Range) = {
		if (exclRange.isEmpty || !(ranges exists (rangeIntersects(_, exclRange))))
			this
		else
			new RangeIntSet(excludeRange(ranges, exclRange))
	}

	private def excludeRange(original: Set[Range], exclRange: Range) =
		original flatMap (r => {
			if (rangeContains(exclRange, r))
				Set.empty[Range]

			else if (rangeIntersects(exclRange, r))
				chop(r, exclRange)

			else
				Set(r)
		})

	private def chop(r: Range, excl: Range): Set[Range] = {
		var result = Set.empty[Range]
		assert(rangeIntersects(r, excl))
		if (r.start < excl.start) {
			result += new Range(r.start, excl.start, 1)
		}
		if (excl.last < r.last){
			result += new Range(excl.last+1, r.last+1, 1)
		}
		result
	}

	private def rangeContains(container: Range, containee: Range) =
		(container.start <= containee.start) && (container.last >= containee.last)

	private def rangeIntersects(r1: Range, r2: Range) =
		(r1.start < r2.last+1) && (r1.last+1 > r2.start)
}

object RangeIntSet {

	val empty = new RangeIntSet(Set.empty[Range])

	def apply(v: Int): RangeIntSet = apply(new Range(v, v+1, 1))
	def apply(vs: Range): RangeIntSet = new RangeIntSet(Set(vs))
	implicit def apply(vs: Set[Int]) = empty ++ vs
}
