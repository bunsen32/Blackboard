/*
 * CellEditor.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import model._
import selection._
import Origin._

/**
 * Reusable class (one per CanvasView) for editing cell contents.
 */
class CellEditor(cell: DataCellInstance) extends GridEditSource {
	val array = cell.part.model.array
	val coords = cell.coords

	def read = array(coords).toString
	def write(s: String) {array(coords) = s}
	def getBounds(canvas: ViewCanvas) = cell.bounds
}
