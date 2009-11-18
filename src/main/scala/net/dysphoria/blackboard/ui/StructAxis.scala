/*
 * StructAxis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable

/**
 * The 'type' of a structure.
 */
class StructAxis(initialLength: Int) extends Axis {
	//class Element(var name: String)

	val elements = new mutable.ArrayBuffer[String]()
	def label(i: Int) = elements(i)
	def length = elements.length

	for(i <- 0 until initialLength) internalInsert(i)

	def internalInsert(index: Int) {
		elements.insert(index, "label" + (length + 1))
	}

	def internalDelete(index: Int) {
		assert(length >= 1)
		elements.remove(index)
	}
}
