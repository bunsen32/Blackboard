/*
 * Table.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import net.dysphoria.blackboard._
import data.{types=>t}

abstract class DataArray {
	def axes = dimensions
	def dimensions: Seq[ArrayAxis]
	def elementType: t.Type
	def arity = dimensions.length

	def apply(c: Map[Axis, Int]): Any

	def update(c: Map[Axis,Int], value: Any): Unit


	//def partiallyApply(c: Coord): Array

	// some kind of observer pattern to observe dimensions, and modify table.

	/**
	 * Break this single table into multiple tables, along the given dimension.
	 */
	//def shatter(d: ArrayAxis): Seq[Array]

	//def addDimension(d: ArrayAxis)

	//def setElementType(e: t.Type)
}
