/*
 * LabelSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.ui.Axis

/**
 * A single selected axis label
 */
case class LabelSelection(coords: Map[Axis, Int]) extends Selectable {
}
