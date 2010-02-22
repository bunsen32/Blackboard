/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.Origin._
import org.eclipse.swt.graphics.{Rectangle, Point}

abstract class DataSelection extends Selectable {
	/**
	 * Every DataSelection is nested within another, except the special
	 * RootDataSelection.
	 */
	def container: DataSelection

	/**
	 * If this is a multiple selection, might have a set or range of values for particular
	 * axes. This method returns the coordinates which are unambiguous,
	 * i.e., which have only a single value per axis.
	 */
	def unambiguousCoords: Map[Axis, Int]

	/**
	 * Defined to be the same as (bounds.x, bounds.y).
	 */
	def position: Point

	/**
	 * Current layout box of this selection. Note that this function may return
	 * different values for the same object if it moves between calls.
	 */
	def bounds: Rectangle
}

object RootDataSelection extends DisplayableContainer {
	def container = throw new InternalFault("RootDataSelection does not itself have a container")
	def unambiguousCoords = Map.empty
	def positionOf(ob: DisplayableInstance) = Origin

	def position = Origin
	def bounds = error("RootDataSelection does not itself have bounds")
}