/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Point

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