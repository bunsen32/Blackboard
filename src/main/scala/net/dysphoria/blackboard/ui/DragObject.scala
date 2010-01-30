package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Point

/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 29-Jan-2010
 * Time: 14:52:11
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class DragObject {
	def isMouseDown: Boolean
	def isStarted: Boolean
	def cancel
}

abstract class ConcreteDragObject extends DragObject {
	def isMouseDown = true
	private var _isStarted = false
	def isStarted = _isStarted

	final def start {
		_isStarted = true
		setUp
	}
	def dragTo(p: Point)
	final def commit {
		drop
		tearDown
	}
	final def cancel {
		tearDown
	}

	protected def setUp {}
	protected def drop
	protected def tearDown {}
}

object NoDrag extends DragObject {
	def isMouseDown = false
	def isStarted = false
	def cancel {}
}