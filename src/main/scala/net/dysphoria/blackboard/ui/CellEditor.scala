/*
 * CellEditor.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Text, Listener}

/**
 * Reusable class (one per CanvasView) for editing cell contents.
 */
class CellEditor(canvas: ViewCanvas) {
	var _visible = false
	var array: ArrayTable = null
	var coords: Map[Axis,Int] = Map.empty
	val input = new Text(canvas, SWT.BORDER)
	input.setVisible(false)

	def addListener(evt: Int, listener: Listener) =
		input.addListener(evt, listener)

	def beginEdit(arr: ArrayTable, c: Map[Axis,Int]){
		if (visible) { save; endEdit }
		array = arr
		coords = c
		input.setText(arr(c).toString)
		visible = true
		input.setFocus
	}

	def save = {
		if (visible)
			array(coords) = input.getText
	}

	def endEdit = {
		save
		visible = false
	}


	def leftPosition =
		if (input.getOrientation == SWT.LEFT_TO_RIGHT) 0 else input.getCharCount

	def rightPosition =
		if (input.getOrientation == SWT.LEFT_TO_RIGHT) input.getCharCount else 0


	private def visible_=(vis: Boolean){
		if (vis != _visible) {
			_visible = vis
			if (vis){
				canvas.geometryChangedListeners += geomListener
			}else{
				canvas.geometryChangedListeners -= geomListener
			}
			input.setVisible(vis)
			updateGeometry
		}
	}
	def visible = _visible
	private val geomListener: canvas.GeometryChangedListener = (_) => updateGeometry

	def updateGeometry {
		if (visible){
			val bounds = canvas.table.cellBounds(coords)
			val topLeft = canvas.modelToView(bounds.x, bounds.y)
			input.setBounds(topLeft.x,
							topLeft.y,
							(bounds.width * canvas.scale).toInt,
							(bounds.height * canvas.scale).toInt)
		}
	}

}
