/*
 * CellSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.proto

import ui.selection.Selectable

case class CellSelection(coords: Map[Axis, Int]) extends Selectable {

}
