/*
 * SingleGridSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

/**
 * A single grid cell (either label or data cell).
 */
trait SingleGridSelection extends Selectable {
	val coords: Map[Axis, Int]
	def hintCoords = coords
}
