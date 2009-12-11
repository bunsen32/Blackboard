/*
 * CellSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.ui.Axis

/**
 * A single selected data cell.
 */
case class CellSelection(coords: Map[Axis, Int]) extends SingleGridSelection {
	def withinData = !coords.exists(pair => pair._2 >= pair._1.length)
}