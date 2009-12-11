/*
 * DimensionAxis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

class ArrayAxis(initialLength: Int) extends Axis {
	require(initialLength >= minimumLength)

	var length = initialLength
	def label(i: Int) = (i + 1).toString

	def internalInsert(index: Int, count: Int) {
		length += count
	}

	def internalDelete(index: Int, count: Int) {
		assert(length >= count)
		length -= count
	}

	def minimumLength = 0
}
