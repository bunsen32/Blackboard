/**
 *  Created by IntelliJ IDEA.
 * User: andrew
 * Date: 08-Jan-2010
 * Time: 21:02:58
 * To change this template use File | Settings | File Templates.
 */

package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui.model._
import ui.{Orientation, CompassPosition}

trait TableItemSelection extends DataSelection { self: DataSelection =>
	def container: DataSelection
	def table: Table#TableInstance
}

abstract class TableSubItemSelection extends TableItemSelection {
	override def container: TableItemSelection
}

trait LabelContainer extends TableItemSelection {
}

trait TableBuildingBlock extends TableItemSelection {
	type ComponentType
	type UnnestedLabelInstance = LabelInstance with UnnestedLabelSelection

	/**
	 * Given a cell in the ‘content area’ of this TableBuildingBlock, return Some SingleGridSelection (or None)
	 * which is the result of moving one cell in the given CompassDirection. We skip over the cell’s own label area (if any),
	 * but hit the adjacent cell’s label area (if any).
	 */
	def oneBeyond(child: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection]

	/**
	 * Move beyond the content, and possibly hit a label in given direction (otherwise the same as oneBeyond). If ‘cell’
	 * does not have a (distinct) label area in that direction, acts same as oneBeyond..
	 */
	def oneBeyondContent(child: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection]

	/**
	 * Given that ‘childPartLabel’ belongs to the (distinct) label area of its container, which itself is a child of this object, move one step from the
	 * label area in the given direction. (Does not make sense for TableArrays since cells of TableArrays do not have their own label areas.)
	 */
	def oneBeyondLabelArea(childPartLabel: UnnestedLabelInstance, direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection]

}