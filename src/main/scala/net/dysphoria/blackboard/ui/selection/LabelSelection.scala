/*
 * LabelSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.ui.Axis

/**
 * A single selected axis label. Includes
 */
case class LabelSelection(block: TableBlock, orientation: Orientation, coords: Map[Axis,Int]) extends SingleGridSelection {
	val actualB = 0
}
