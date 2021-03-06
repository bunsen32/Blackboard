/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Rectangle
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Text, Listener}

class GridTextEditor(canvas: ViewCanvas) {
	var source: GridEditSource = null

	var _visible = false
	val input = new Text(canvas, SWT.BORDER | SWT.CENTER)
	input.setVisible(false)

	def addListener(evt: Int, listener: Listener) =
		input.addListener(evt, listener)

	def beginEdit(newSource: GridEditSource){
		if (visible) { save; endEdit }
		source = newSource
		val text = source.read
		input.setText(text)
		// Select all the text. Mac does this by default, but Win doesn’t
		input.setSelection(0, text.length)
		visible = true
		input.setFocus
	}

	def save = {
		if (visible)
			source.write(input.getText)
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
			val bounds = source.getBounds(canvas)
			val topLeft = canvas.modelToView(bounds.x, bounds.y)
			input.setBounds(topLeft.x,
							topLeft.y,
							(bounds.width * canvas.scale).toInt,
							(bounds.height * canvas.scale).toInt)
		}
	}

}
