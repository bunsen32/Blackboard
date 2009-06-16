package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._
import blackboard.data

class DisplayDimension(val dim: data.Dimension) {
	var defaultItemWidth: Int = 10
	var interItemLine: Option[LineDescriptor] = None
	
	def width = defaultItemWidth * dim.length
	def itemWidth(index: Int):Int = defaultItemWidth

	override def toString = "disp" + dim.toString
}

object DisplayDimension {

	def widthOf(dims: List[DisplayDimension]): Int = dims match {
		case dim :: Nil => dim.width
		case dim :: others => dim.dim.length * widthOf(others)
		case Nil => throw new IllegalArgumentException("Dimension list is empty")
	}

	def itemWidthOf(dims: List[DisplayDimension], indexes: Seq[Int]): Int = dims match {
		case dim :: Nil => dim.defaultItemWidth
		case dim :: others => widthOf(others)
		case Nil => throw new IllegalArgumentException("Dimension list is empty")
	}

	def hitTest(dims: List[DisplayDimension], p: Int): Option[(Seq[Int], Int)] = {
		if (p >= 0 && p < widthOf(dims))
			Some(hitTestSafe(dims, p, Nil))
		else
			None
	}

	def hitTestSafe(dims: List[DisplayDimension], p: Int, indexes: List[Int]): (List[Int], Int) = dims match {
		case dim :: others => {
			val w = itemWidthOf(dims, indexes)
			val ix = p / w
			assert(ix >= 0);
			assert(ix < dim.dim.length)
			val offset = p - (ix * w)
			val (ixs, remainder) = hitTestSafe(others, offset, ix :: indexes)
			(ix :: ixs, remainder)
		}
		case Nil => {
			(Nil, p)
		}
	}

}
