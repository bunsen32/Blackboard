/*
 * CellSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.ui.{Axis, TableBlock}

/**
 * A single selected data cell.
 */
case class CellSelection(table: Table, coords: Map[Axis, Int]) extends SingleGridSelection {
	def withinData = !coords.exists(pair => pair._2 >= pair._1.length)

	lazy val (hLabel, vLabel) = {
		// Horendously complex code for what is a simple concept: identify the two
		// OneLabel elements which uniquely identify the outer CellSelection.
		def recurse(parentCoordsH: Map[Axis, Int], parentCoordsV: Map[Axis, Int], block: TableBlock, hSoFar: Option[OneLabel], vSoFar: Option[OneLabel]): (Option[OneLabel], Option[OneLabel]) = {
			@inline def blockToLabel(soFar: Option[OneLabel], o: Orientation, parentCoords: Map[Axis,Int]) = {
				val axes = block.axes(o)
				val thisCoords = parentCoords ++ axes.map(ax => (ax, coords(ax)))
				val sel = if (axes.isEmpty) soFar else Some(OneLabel(block, o, thisCoords))
				(thisCoords, sel)
			}
			val (thisCoordsH, h) = blockToLabel(hSoFar, Horizontal, parentCoordsH)
			val (thisCoordsV, v) = blockToLabel(vSoFar, Vertical, parentCoordsV)
			block match {
				case s: StructBlock =>
					val nextParentCoordsH = thisCoordsH ++
						(if (s.orientation == Vertical) thisCoordsV else Nil)

					val nextParentCoordsV = thisCoordsV ++
						(if (s.orientation == Horizontal) thisCoordsH else Nil)

					recurse(nextParentCoordsH, nextParentCoordsV, s.elementFor(coords), h, v)

				case _: ArrayBlock => (h, v)
			}
		}
		recurse(Map.empty, Map.empty, table.topBlock, None, None)
	}
}