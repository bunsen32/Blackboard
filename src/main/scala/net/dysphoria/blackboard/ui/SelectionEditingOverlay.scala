/*
 * MutatorOverlay.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import selection._
import org.eclipse.swt.graphics.Rectangle

/**
 * Displays a set of overlay icons for insertions/
 */
class SelectionEditingOverlay(val control: ViewCanvas) {
	private var _visible = false
	private var _selection: Option[Selectable] = None
	private var _rect: Rectangle = null

	control.geometryChangedListeners += (_ => reposition)
	val insertRight = new InsertOverlay(control.getShell){val rotation = 0D}

	def visible = _visible
	def visible_=(v: Boolean) {
		_visible = v
		insertRight.visible = v
		reposition
	}

	def selection = _selection
	def selection_=(s: Selectable) {
		_selection = Some(s)

		_rect = s match {
			case cell: CellSelection =>
				control.table.cellBounds(cell.coords)
			case lab: LabelSelection =>
				control.table.labelBounds(lab)
			case _ =>
				new Rectangle(0, 0, 10, 10)
		}
		reposition
	}

	def reposition {
		if (visible) {
			val topLeft = control.toDisplay(control.modelToView(_rect.x, _rect.y))
			val bottomRight = control.toDisplay(control.modelToView(_rect.x + _rect.width, _rect.y + _rect.height))
			insertRight.setLocation(bottomRight.x, topLeft.y)
		}
	}
}
