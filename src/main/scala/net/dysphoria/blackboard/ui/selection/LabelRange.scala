/*
 * LabelRange.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

case class LabelRange(block: TableBlock, orientation: Orientation, allCoordsButLast: Map[Axis,Int], axis: Axis, range: Range) extends LabelSelection {
	lazy val parentCoords = allCoordsButLast -- (block.axes(orientation))
}
