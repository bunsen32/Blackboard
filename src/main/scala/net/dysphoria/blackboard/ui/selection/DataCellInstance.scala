/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard.ui.Axis
import net.dysphoria.blackboard.ui.model.TableArrayData

case class DataCellInstance(part: TableArrayData#Instance, coords: Map[Axis, Int]) extends AbstractCellInstance {
	def adjacent(direction: CompassPosition)(implicit hint: SelectionHints) = part.oneBeyond(this, direction)
}
