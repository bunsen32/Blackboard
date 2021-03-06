/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.data

abstract class Table extends Function {
	def applyByIndex(ixs: Seq[Int]): Any
	def dimensions: Seq[Dimension]
	def valueToString(value: Any) = value.toString
}

abstract class ByIndexTable extends Table {
	def apply(args: Seq[Any]) = applyByIndex(valuesToIndices(args, 0))

	def valuesToIndices(args: Seq[Any], i: Int): List[Int] =
		if (i == arity)
			Nil
		else
			dimensions(i).inverse.apply1(args(i)).asInstanceOf[Int] ::
				valuesToIndices(args, i + 1)
}

abstract class ByValueTable extends Table {
	def applyByIndex(ixs: Seq[Int]) = apply(indicesToValues(ixs, 0))

	def indicesToValues(args: Seq[Int], i: Int): List[Any] =
		if (i == arity)
			Nil
		else
			dimensions(i).apply1(args(i)) ::
				indicesToValues(args, i + 1)
}