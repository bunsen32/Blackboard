package net.dysphoria.blackboard.data


/**
 * A Dimension is an orthogonal extent of a table. Generally tables may have
 * &gt;1 dimension. A Dimension is itself a table. Also, for efficiency, we
 * generally refer to a coordinate along a dimension by its index rather than
 * by its value.
 */
abstract class Dimension extends Table with Bijection1 {
 	def length: Int
	def domain: Range
	def range: Collection[Any]
	def applyByIndex(args: Seq[Int]) = apply1(args(0))
	def apply1(ix: Any) = apply1(ix.asInstanceOf[Int])
	def apply1(ix: Int): Any
	def inverse1(value: Any) = inverse.apply1(value).asInstanceOf[Int]

	override lazy val dimensions = List(new DimensionDomainDimension(length))
}

/**
 * A point along a dimension.
 */
final class DimensionValue(val dimension: Dimension, val index: Int){
	require(dimension.domain contains index)
	def this(dim: Dimension, value: Any) = this(dim, dim.inverse1(value))
	def value = dimension.apply1(index)
}

/**
 * One common and useful class of Dimensions consists of a contiguous range
 * of integers.
 */
abstract class IntegerRangeDimension(override val range: Range) extends Dimension { dim =>
	override def length = range.length
	override def domain = (0 until length)
	override def apply1(in: Int) = range(in)
}

class ConcreteIntegerRangeDimension(range: Range) extends IntegerRangeDimension(range) { dim =>
	override val inverse = new Bijection1 {
		override def apply1(ix: Any) = {
			val i = ix.asInstanceOf[Int]
			require(range.contains(i))
			i - range.start
		}
		override val inverse = dim
	}

	override def toString = "IntegerRange("+range+")"
}


/**
 * For uniformity, a Dimension is itself a (1-dimensional) table, mapping integers
 * (starting from 0) to values. This class is the dimension of a Dimension of
 * length <var>length</var>.
 */
class DimensionDomainDimension(override val length: Int) extends IntegerRangeDimension(0 to length){
	override val inverse: Bijection1 = this
	override def inverse1(ix: Any) = ix.asInstanceOf[Int]
	override lazy val dimensions = List(this)
}


/**
 * The dimension of length 1, with single value, ‘()’ (Scala Unit). What is it
 * good for? Use it as a ‘stand-in’ dimension.
 */
object UnitDimension extends Dimension { dim =>
	override def length = 1
	override def domain = (0 to 0)
	override def range = () :: Nil
	override def apply1(in: Int) = in match {
		case 0 => ()
		case _ => throw new IllegalArgumentException
	}

	override val inverse = new Bijection1 {
		override def apply1(in: Any) = in match {
			case () => 0
			case _  => throw new IllegalArgumentException
		}
		override val inverse = dim
	}

	override def toString = "UnitDimension"
}
