/*
 * Axis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import net.dysphoria.blackboard._
import gfx.LineDescriptor

abstract class Axis {
	def length: Int
	def range = 0 until length
	def label(i: Int): String
	def label_=(i: Int, s: String)
	
	/**
	 * Certain axis labels are drawn as ‘greyed-out’. For struct axes these are
	 * ‘hidden’ axis labels; for array axes, these are numbered labels where there
	 * exist some manually-entered labels.
	 */
	def greyed(i: Int): Boolean
	def first = 0
	def last = length - 1

	var interItemLine: Option[LineDescriptor] = None

	def insert(index: Int, count: Int) {
		require(count > 0)
		require(index >= 0 && index <= length)
		internalInsert(index, count)
		notifyAxisChangedListeners(index, 0, count)
	}
	def delete(index: Int, count: Int) {
		require(count > 0)
		require(index >= 0 && index <= (length - count))
		require(length - count >= 0)
		internalDelete(index, count)
		notifyAxisChangedListeners(index, count, 0)
	}

	protected def internalInsert(index: Int, count: Int)
	protected def internalDelete(index: Int, count: Int)

	/**
	 * Normally you want to delete the axis when number of elements is less than this.
	 */
	def nominalMinimumLength: Int

	type AxisChangedListener = Function4[Axis,Int,Int,Int,Unit]
	val axisChangedListeners = new mutable.HashSet[AxisChangedListener]
	protected def notifyAxisChangedListeners(position: Int, numberRemoved: Int, numberAdded: Int) =
		for(l <- axisChangedListeners)
			l(this, position, numberRemoved, numberAdded)
}
