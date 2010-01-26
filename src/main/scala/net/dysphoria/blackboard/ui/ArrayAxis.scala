/*
 * DimensionAxis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable

class ArrayAxis(initialLength: Int) extends Axis {
	require(initialLength >= nominalMinimumLength)

	var length = initialLength
	val labels = new mutable.ArrayBuffer[String]
	var labelsCount = 0
	def defaultLabel(ix: Int) = (ix + 1).toString
	
	def label(ix: Int) =
		if (ix < labels.length && labels(ix) != null)
			labels(ix)
		else
			defaultLabel(ix)

	def greyed(ix: Int) =
		(labelsCount != 0) &&
			((ix >= labels.length) ||
			 (labels(ix) == null))

	def label_=(ix: Int, str: String) = {
		require(ix >= 0 && ix < length)
		lazy val default = defaultLabel(ix)
		str match {
			case null | "" | `default` =>
				if (ix < labels.length && labels(ix) != null){
					labelsCount -= 1
					labels(ix) = null
				}
			case _ =>
				while(labels.length < ix + 1) labels.append(null)
				if (labels(ix) == null) labelsCount += 1
				labels(ix) = str
		}
	}


	def internalInsert(index: Int, count: Int) {
		length += count
		if (index < labels.length)
			for(_ <- 0 until count) labels.insert(index, null)
	}

	def internalDelete(index: Int, count: Int) {
		assert(length >= count)
		length -= count
		val labelsToRemove = (labels.length - index) min count
		for(_ <- 0 until labelsToRemove) {
			if (labels(index) != null) labelsCount -= 1
			labels.remove(index)
		}
	}

	def nominalMinimumLength = 0

	override def toString = "ArrayAxis("+length+")"
}
