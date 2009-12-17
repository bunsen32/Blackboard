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
	def table: Table
	val coords: Map[Axis, Int]
	def hintCoords = coords
	
	/**
	 * Whether any index is beyond the range of its axis. An index should only
	 * ever be 1 eyond the end of the axis (to allow representation of the space
	 * at the end of the axis).
	 */
	def withinData: Boolean
}
