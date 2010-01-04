/*
 * CellEditor.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

/**
 * Reusable class (one per CanvasView) for editing cell contents.
 */
class CellEditor(array: ArrayTable, coords: Map[Axis,Int]) extends GridEditSource {
	def read = array(coords).toString
	def write(s: String) {array(coords) = s}
	def getBounds(canvas: ViewCanvas) = canvas.table.cellBounds(coords)
}
