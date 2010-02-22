/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import model.Group
import org.eclipse.swt.graphics.Point
import Origin._

abstract class GroupElementDrag(element: DisplayableInstance, dragHandleModel: Point) extends ConcreteDragObject {
	val group = element.container.asInstanceOf[Group#Instance]
	var dragHandleOffset: Point = Origin
	val originalRelativePosition = group.model.getRelativePosition(element.model)

	override protected def setUp {
		val elementPosition = element.position
		dragHandleOffset = new Point(
			dragHandleModel.x - elementPosition.x,
			dragHandleModel.y - elementPosition.y)
	}

	protected def drop {	}

	def dragTo(p: Point) {
		val newPosition = new Point(p.x - dragHandleOffset.x, p.y - dragHandleOffset.y)
		if (newPosition.x >= 0 && newPosition.y >= 0){
			group.model.setRelativePosition(element.model, newPosition)
			modelUpdated
		}
	}

	def modelUpdated
}