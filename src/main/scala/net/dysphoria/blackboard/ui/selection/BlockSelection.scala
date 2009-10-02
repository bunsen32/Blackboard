/*
 * CellSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.ui.Axis
/**
 * Not a single cell, but a block of cells and headers.
 */
case class BlockSelection(coords: Map[Axis, Int]) extends Selectable {

}
