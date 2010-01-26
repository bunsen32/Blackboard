package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard.ui.Axis
import net.dysphoria.blackboard.ui.model.TableArrayContainer

/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 10-Jan-2010
 * Time: 20:27:21
 * To change this template use File | Settings | File Templates.
 */

case class ContainerCellInstance(part: TableArrayContainer#Instance, coords: Map[Axis, Int]) extends AbstractCellInstance {
	def contents = part.model.contents.instance(coords)

	def adjacent(direction: CompassPosition)(implicit hint: SelectionHints) = part.oneBeyond(this, direction)
}
