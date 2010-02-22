/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.model._

/**
 * A single selected data cell.
 */
abstract class AbstractCellInstance extends TableSubItemSelection with SingleGridSelection {
	val part: TableArray#TableArrayInstance
	def container = part
	def withinData = !coords.exists(pair => pair._2 >= pair._1.length)
	def table = part.table

	lazy val hLabel = findLabel(part, Horizontal)
	lazy val vLabel = findLabel(part, Vertical)
	
	private def findLabel(thePart: TablePart#TablePartInstance, orientation: Orientation): Option[LabelInstance] = {
		val axesList = thePart.axes(orientation)
		val containerLabel = thePart.container match {
				case parentPart: TableStruct#TableStructInstance
					if axesList.isEmpty || parentPart.model.orientation == orientation =>
						findLabel(parentPart, orientation)
				case _ =>
					None
			}
		if (axesList.isEmpty)
			containerLabel
		else {
			val container = containerLabel.getOrElse(thePart)
			val appropriateCoords = container.unambiguousCoords ++ (for(ax <- axesList) yield (ax, coords(ax)))
			Some(LabelInstance(
				container,
				thePart.model,
				orientation,
				appropriateCoords))
		}
	}

}
