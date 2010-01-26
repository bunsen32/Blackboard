/*
 * Displayable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import net.dysphoria.blackboard._
import gfx._
import ui.selection._

abstract class Displayable {
	def computeSize
	def render(g: DrawingContext, xy: Point, context: Map[Axis, Int]);
	def size: Point
	def boundsOf(origin: Point, ob: DataSelection): Rectangle

	type Instance <: DisplayableInstance

	def instance(coords: Map[Axis,Int]): Instance
}

abstract class DisplayableInstance extends DataSelection {
	def displayable: this.type = this
	def model: Displayable
	val coords: Map[Axis, Int]
	final def unambiguousCoords = coords
	def hitTest(p: Point): Selectable = NullSelection
}
