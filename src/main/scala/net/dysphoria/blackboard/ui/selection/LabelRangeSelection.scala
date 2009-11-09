/*
 * LabelRangeSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

case class LabelRangeSelection(block: TableBlock, orientation: Orientation, coords: Map[Axis,Int], axis: Axis, range: Range) extends Selectable
