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
case class CellSelection(coords: Map[Axis, Int]) extends Selectable {

}
