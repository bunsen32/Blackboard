/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard.ui
import ui.{Axis, DisplayableContainer}
import ui.model.TableArrayContainer

case class ContainerCellInstance(part: TableArrayContainer#Instance, coords: Map[Axis, Int]) extends AbstractCellInstance with DisplayableContainer {
	def contents = part.model.contents.instance(this, coords)

	def adjacent(direction: CompassPosition)(implicit hint: SelectionHints) = part.oneBeyond(this, direction)

	def positionOf(ob: DisplayableInstance) = {
		this.position
	}

}
