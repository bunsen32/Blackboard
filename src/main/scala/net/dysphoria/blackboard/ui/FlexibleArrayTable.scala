/*
 * FlexibleArrayTable.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import data.{types=>t}

class FlexibleArrayTable(initialDims: Seq[ArrayAxis]) extends ArrayTable {
	val elementType = t.core.String

	private var _dimensions: Array[ArrayAxis] = initialDims.toArray
	private var _data: mutable.Map[Array[Int], Any] = new mutable.HashMap

	def dimensions = _dimensions

	def addDimension(dim: ArrayAxis){
		require(!(_dimensions contains dim))
		val oldNumberOfDims = _dimensions.length
		val newNumberOfDims = oldNumberOfDims + 1
		_dimensions = _dimensions ++ Some(dim)

		remapDimensions(oldK => {
			val newK = new Array[Int](newNumberOfDims)
			Array.copy(oldK, 0, newK, 0, oldNumberOfDims)
			newK(oldNumberOfDims) = 0
			newK})

		dim.axisChangedListeners += axisChanged
	}

	def removeDimension(dim: ArrayAxis){
		require(_dimensions contains dim)
		val dimIndex = (_dimensions indexOf dim)
		val newNumberOfDims = _dimensions.length - 1
		val newDimensions = new Array[ArrayAxis](newNumberOfDims)
		for(i <- 0 until newNumberOfDims if i != dimIndex)
			newDimensions(if (i < dimIndex) i else i - 1) = _dimensions(i)

		remapDimensions(oldK => {
			if (oldK(dimIndex) != 0)
				null
			else {
				val newK = new Array[Int](newNumberOfDims)
				for(i <- 0 until newNumberOfDims if i != dimIndex)
					newK(if (i < dimIndex) i else i - 1) = oldK(i)
				newK
			}
		})

		dim.axisChangedListeners -= axisChanged
	}

	private val axisChanged = (axis: Axis, index: Int, deleted: Int, added: Int) => {
		// TODO!
		// Remap dimensions to accommodate inserted or deleted values.
	}

	private def remapDimensions(keyFunc: Array[Int]=>Array[Int]) {
		var newData = new mutable.HashMap[Array[Int], Any]
		for((k, v) <- _data)
			keyFunc(k) match {
				case null => // do nothing
				case newKey => newData(newKey) = v
			}
		_data = newData
	}

	def apply(coords: Map[Axis,Int]) = _data.getOrElse(flatten(coords), "")

	def update(coords: Map[Axis,Int], value: Any) {
		_data(flatten(coords)) = value
	}

	def flatten(coords: Map[Axis,Int]) =
		for(d <- dimensions) yield coords(d)

}
