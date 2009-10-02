/*
 * Navigator.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import selection._

abstract class Navigator {
	val topTable: Block

	def moveByCell(sel: CellSelection, o: Orientation, delta: Int) = {
		def moveHelper(b: Block): Selectable = {

			def nextOnAxes(axes: Seq[Axis]) = {
				def next(axes: Seq[Axis], old: Map[Axis, Int]): Selectable = {
					if (axes.isEmpty)
						NullSelection
					else {
						val axis = axes.first
						val p = sel.coords(axis)
						val max = axis.length
						val newP = p + delta
						if (newP < 0)
							// Before start of axis. Wrap around and move onto next axis:
							next(axes.drop(1), old + (axis -> axis.last))

						else if (newP < axis.length)
							// Within range. Accept new value:
							CellSelection(old + (axis -> newP))

						else
							// After end of axis. Wrap around and move onto next axis:
							next(axes.drop(1), old + (axis -> axis.first))
					}
				}
				// We increment least-significant axis first, moving onto more-
				// significant axes if we hit the end of the range.
				next(axes.reverse, sel.coords)
			}

			b match {
				case str: StructBlock =>
					val structAxis = str.structAxis
					val el = str.elements(sel.coords(structAxis))
					moveHelper(el).orElse(
						nextOnAxes(str.axes(o)) match {
							case CellSelection(c) =>
								val el = str.elements(c(structAxis))
								val subCoords = if (delta > 0) first(el, o) else last(el, o)
								CellSelection(c ++ subCoords)
								
							case s=>s
						}
					)

				case arr: ArrayBlock =>
					val axes = arr.axes(o)
					nextOnAxes(axes)
			}
		}
		moveHelper(topTable)
	}


	def first(b: Block, o: Orientation): Map[Axis, Int] = (
		Map.empty
		++ (b.axes(o).map(a => (a -> a.first)))
		++ (b match {
				case s: StructBlock if s.orientation == o =>
					first(s.elements(s.structAxis.first), o)
				case _ => Nil
			}))

	def last(b: Block, o: Orientation): Map[Axis, Int] = (
		Map.empty
		++ (b.axes(o).map(a => (a -> a.last)))
		++ (b match {
				case s: StructBlock if s.orientation == o =>
					last(s.elements(s.structAxis.last), o)
				case _ => Nil
			}))



}
