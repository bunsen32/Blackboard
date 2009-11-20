/*
 * LabelRange.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

case class LabelRange(block: TableBlock, orientation: Orientation, allCoordsButLast: Map[Axis,Int], axis: Axis, range: Range) extends LabelSelection {
	require(range.length > 1, "LabelRange consisting of fewer than 2 labels not allowed")
	lazy val parentCoords = allCoordsButLast -- (block.axes(orientation))
	lazy val axisIndex =
		block.axes(orientation)
		.takeWhile(allCoordsButLast.contains(_))
		.length

	lazy val first = OneLabel(block, orientation, allCoordsButLast + (axis -> range.first))
	lazy val last = OneLabel(block, orientation, allCoordsButLast + (axis -> range.last))
}
