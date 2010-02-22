/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import scala.collection.mutable

/**
 * The 'type' of a structure.
 */
class StructAxis(initialLength: Int) extends Axis {
	val elements = new mutable.ArrayBuffer[(String, Boolean)]()
	def label(i: Int) = elements(i)._1
	def visible(i: Int) = elements(i)._2
	def label_=(ix: Int, s: String) = {
		elements(ix) = (s, visible(ix))
	}
	def greyed(ix: Int) = !visible(ix)
	def length = elements.length

	require(initialLength >= 0)
	internalInsert(0, initialLength)

	def internalInsert(index: Int, count: Int) {
		for(n <- 0 until count)
			elements.insert(index + n, ("label" + (length + 1), true))
	}

	def internalDelete(index: Int, count: Int) {
		assert(length >= count)
		for(n <- 0 until count)
			elements.remove(index)
	}

	def nominalMinimumLength = 1
	
	override def toString = elements.map(_._1).mkString("StructAxis(", ",", ")")
}
