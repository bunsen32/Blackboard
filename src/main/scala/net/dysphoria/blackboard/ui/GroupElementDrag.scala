package net.dysphoria.blackboard.ui

import model.Group
import org.eclipse.swt.graphics.Point
import Origin._

/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 29-Jan-2010
 * Time: 15:16:19
 * To change this template use File | Settings | File Templates.
 */

class GroupElementDrag(element: DisplayableInstance, dragHandleModel: Point) extends ConcreteDragObject {
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
		group.model.setRelativePosition(element.model, newPosition)
		// TODO: recalculate bounds all the way down.
	}
}