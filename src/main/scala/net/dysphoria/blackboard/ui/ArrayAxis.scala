/*
 * DimensionAxis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

class ArrayAxis(initialLength: Int) extends Axis {
	var length = initialLength
	def label(i: Int) = (i + 1).toString

	def internalInsert(index: Int) {
		length += 1
	}

	def internalDelete(index: Int) {
		assert(length >= 1)
		length -= 1
	}
}
