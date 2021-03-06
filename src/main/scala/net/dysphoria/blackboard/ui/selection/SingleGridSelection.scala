/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.model._
import org.eclipse.swt.graphics.Point

/**
 * A single grid cell (either label or data cell).
 */
trait SingleGridSelection extends TableSubItemSelection {
	val coords: Map[Axis, Int]
	final def unambiguousCoords = coords	
	
	/**
	 * Whether any index is beyond the range of its axis. An index should only
	 * ever be 1 beyond the end of the axis (to allow representation of the space
	 * at the end of the axis).
	 */
	def withinData: Boolean

	/**
	 * Returns the data cell or label in the given direction, or else None.
	 */
	def adjacent(direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection]

	def bounds = table.boundsOf(this)

	def position = {
		val b = bounds
		new Point(b.x, b.y)
	}
}
