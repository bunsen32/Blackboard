/*
 * LabelSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.proto

import ui.selection.Selectable

case class LabelSelection(coords: Map[Axis, Int]) extends Selectable {
}
