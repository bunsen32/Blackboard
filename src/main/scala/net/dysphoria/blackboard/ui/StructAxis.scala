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
	val elements = new mutable.ArrayBuffer[(String, Boolean)]()
	def label(i: Int) = elements(i)._1
	def visible(i: Int) = elements(i)._2
	def length = elements.length

	require(initialLength >= minimumLength)
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

	def minimumLength = 1
	
	override def toString = elements.map(_._1).mkString("StructAxis(", ",", ")")
}
