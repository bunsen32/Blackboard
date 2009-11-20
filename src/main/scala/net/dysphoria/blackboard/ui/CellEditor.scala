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
class CellEditor(array: ArrayTable, coords: Map[Axis,Int]) extends GridEditSource {
	def read = array(coords).toString
	def write(s: String) {array(coords) = s}
	def getBounds(canvas: ViewCanvas) = canvas.table.cellBounds(coords)
}
