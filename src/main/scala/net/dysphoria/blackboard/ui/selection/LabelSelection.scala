/*
 * LabelSelection.scala.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

abstract class LabelSelection extends Selectable {
	def block: TableBlock
	def orientation: Orientation
	def parentCoords: Map[Axis,Int]
	def allCoordsButLast: Map[Axis,Int]
	def axis: Axis
}
