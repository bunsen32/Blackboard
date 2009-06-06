/*
 * TableCellSelectable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.data._
import scala.collection.immutable
import scala.collection.mutable

trait HasTableCells extends Selectable {
	def numberOfCells: Int
	def isSingleCell: Boolean
	def cellCoords: Coord

	def containsCell(b: TableBlock, coords: Coord): Boolean
	implicit def toCellSet: MultipleTableCellRanges

	override def contains(other: Selectable) = other match {
		case cell: HasTableCell => containsCell(cell.block, cell.cellCoords)
		case _ => super.contains(other)
	}
}

trait HasTableCell extends HasTableCells with HasDisplayBlock {
	override def numberOfCells = 1
	override def isSingleCell = true
	override def containsCell(b: TableBlock, coords: Coord) = (b == block && coords == cellCoords)

	override def block: TableBlock

	implicit def toCellSet = new MultipleTableCellRanges(block, RangeCoordSet(cellCoords))

	def to(other: HasTableCell) = {
		require(other.block == this.block)
		new MultipleTableCellRanges(block, RangeCoordSet(this.cellCoords to other.cellCoords))
	}
}

case class SingleTableCell(override val block: TableBlock, val cellCoords: Coord)
		extends HasTableCell


class MultipleTableCellRanges(val tableCells: Map[TableBlock, Set[Coord]])
		extends HasTableCells /*with HasDisplayBlocks*/ {

	def this(block: TableBlock, initialRange: Set[Coord]) = this(Map(block -> initialRange))

	override def containsCell(b: TableBlock, coords: Coord) =
		tableCells.get(b) match {case None => false; case Some(set) => set contains coords}

	// This will currently fail due to underlying RangeCoordSet implementation
	override def numberOfCells = (0 /: tableCells.values)(_ + _.size)

	override def isSingleCell = false
	override def cellCoords = throw new UnsupportedOperationException("≠ 1 cell")
	override def toCellSet = this


	def +(other: MultipleTableCellRanges): MultipleTableCellRanges =
		(this /: other.tableCells)(_ + _)

	def +(cells: (TableBlock, Set[Coord])) = {
		val (block, newCoords) = cells
		val existingCells = tableCells.getOrElse(block, RangeCoordSet.empty)
		new MultipleTableCellRanges(tableCells.update(block, existingCells ++ newCoords))
	}


	def -(other: MultipleTableCellRanges): MultipleTableCellRanges =
		(this /: other.tableCells)(_ - _)

	
	def -(cells: (TableBlock, Set[Coord])) = {
		val (block, newCoords) = cells
		val existingCells = tableCells.getOrElse(block, RangeCoordSet.empty)
		if (existingCells.isEmpty)
			this
		else
			new MultipleTableCellRanges(tableCells.update(block, existingCells -- newCoords))
	}


}


