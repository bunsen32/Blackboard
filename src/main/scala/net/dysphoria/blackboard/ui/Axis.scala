/*
 * Axis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import gfx.LineDescriptor

abstract class Axis {
	def length: Int
	def range = 0 until length
	def label(i: Int): String
	def first = 0
	def last = length - 1

	var interItemLine: Option[LineDescriptor] = None

	def insert(index: Int) {
		require(index >= 0 && index <= length)
		internalInsert(index)
		notifyAxisChangedListeners(index, 0, 1)
	}
	def delete(index: Int) {
		require(index >= 0 && index < length)
		internalDelete(index)
		notifyAxisChangedListeners(index, 1, 0)
	}

	protected def internalInsert(index: Int)
	protected def internalDelete(index: Int)

	type AxisChangedListener = Function4[Axis,Int,Int,Int,Unit]
	val axisChangedListeners = new mutable.HashSet[AxisChangedListener]
	protected def notifyAxisChangedListeners(position: Int, numberRemoved: Int, numberAdded: Int) =
		for(l <- axisChangedListeners)
			l(this, position, numberRemoved, numberAdded)
}
