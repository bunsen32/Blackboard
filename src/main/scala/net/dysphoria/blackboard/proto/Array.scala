/*
 * Table.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import data.{types=>t}

abstract class Array extends Aggregate {
	def axes = dimensions
	def dimensions: Seq[ArrayAxis]
	def elementType: t.Type
	def arity = dimensions.length
	//def apply(c: Coord): Any
	def apply(c: Map[Axis, Int]) = {
		var p = 0
		for(d<-dimensions){
			val i = c.getOrElse(d, error("Axis "+d+" is not provided"))
			p = (p * d.length) + i
		}
		flatApply(p)
	}
	protected def flatApply(p: Int): Any
	//def partiallyApply(c: Coord): Array

	// some kind of observer pattern to observe dimensions, and modify table.

	/**
	 * Break this single table into multiple tables, along the given dimension.
	 */
	//def shatter(d: ArrayAxis): Seq[Array]

	//def addDimension(d: ArrayAxis)

	//def setElementType(e: t.Type)
}
