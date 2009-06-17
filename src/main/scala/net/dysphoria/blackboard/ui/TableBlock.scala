package net.dysphoria.blackboard.ui

import blackboard.data._
import scala.collection.immutable
import selection._

class TableBlock(table: Table) extends TabularBlock(table) {
	var dimensionMap = immutable.Map.empty[DisplayDimension, Int]

	def requireValidDimensionMap {
		require(dimensionMap.size == table.arity)
		for((dd, ix) <- dimensionMap) {
			require((xDimensions contains dd) || (yDimensions contains dd))
			require(ix >= 0 && ix < table.arity && dd.dim == table.dimensions(ix))
		}
	}

	override def cellIsSelected(ui: UIState, indices: Array[Int]) = ui.selection match {
		case s: HasTableCells => {
			if (s.containsCell(this, indices)) 2 else 0
		}
		case _ => super.cellIsSelected(ui, indices)
	}
}
