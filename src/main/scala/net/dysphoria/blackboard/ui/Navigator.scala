/*
 * Navigator.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import selection._

abstract class Navigator {
	val table: Table


	/*def moveByCell(sel: SingleGridSelection, o: Orientation, incNotDec: Boolean) = {
		def helper(b: TableBlock): Selectable = {
			b match {
				case str: StructBlock =>
				case arr: ArrayBlock => 
			}
		}
	}*/


	/**
	 * Not sure if I’m happy with LabelSelection knowing its own orientation
	 * (cos what happens if the layout changes after selection), so this method
	 * abstracts that away.
	 */
	def orientation(sel: LabelSelection) = sel.orientation
	

	def moveByCell(sel: CellSelection, o: Orientation, incNotDec: Boolean) = {
		
		def moveHelper(b: TableBlock): Selectable = {
			b match {
				case str: StructBlock =>
					val structAxis = str.structAxis
					val el = str.elements(sel.coords(structAxis))
					moveHelper(el).orElse(
						nextOnAxes(str.axes(o), sel.coords, incNotDec) match {
							case Some(c) =>
								val el = str.elements(c(structAxis))
								val subCoords = if (incNotDec) first(el, o) else last(el, o)
								CellSelection(c ++ subCoords)
								
							case None => NullSelection
						}
					)

				case arr: ArrayBlock =>
					val arrayAxes = arr.axes(o)
					nextOnAxes(arrayAxes, sel.coords, incNotDec) match {
						case Some(coords) => CellSelection(coords)
						case None => NullSelection
					}
			}
		}
		moveHelper(table.topBlock)
	}

	
	def moveByOne(sel: LabelSelection, o: Orientation, incNotDec: Boolean) = {
		def helper(parent: Map[Axis,Int], block: TableBlock, coords: Map[Axis,Int]): Selectable = {
			if (block != sel.block) {
				block match {
					case s:StructBlock =>
						val el = s.elements(coords(s.structAxis))
						helper(parent, el, coords)
					case _ => error("[Should not happen.] Selection doesn't contain target block.")
				}
			}else{
				if (o == sel.orientation) {
					val axes = block.axes(o).takeWhile(coords.contains(_))
					nextOnAxes(axes, coords, incNotDec) match {
						case Some(c) => LabelSelection(block, o, c, sel.actualB)
						case None => NullSelection
					}
				}else{
					val i = block.axes(o).indexOf()
					NullSelection
				}
			}
		}

		helper(Map.empty, table.topBlock, sel.coords)
	}


	def nextOnAxes(blockAxes: Seq[Axis], start: Map[Axis,Int], incNotDec: Boolean): Option[Map[Axis,Int]] = {
		val delta = if (incNotDec) 1 else -1
		def next(axes: Seq[Axis], old: Map[Axis, Int]): Option[Map[Axis,Int]] = {
			if (axes.isEmpty)
				None
			else {
				val axis = axes.first
				val p = start(axis)
				val max = axis.length
				val newP = p + delta
				if (newP < 0)
					// Before start of axis. Wrap around and move onto next axis:
					next(axes.drop(1), old + (axis -> axis.last))

				else if (newP < axis.length)
					// Within range. Accept new value:
					Some(old + (axis -> newP))

				else
					// After end of axis. Wrap around and move onto next axis:
					next(axes.drop(1), old + (axis -> axis.first))
			}
		}
		// We increment least-significant axis first, moving onto more-
		// significant axes if we hit the end of the range.
		next(blockAxes.reverse, start)
	}


	def first(b: TableBlock, o: Orientation): Map[Axis, Int] = (
		Map.empty
		++ (b.axes(o).map(a => (a -> a.first)))
		++ (b match {
				case s: StructBlock if s.orientation == o =>
					first(s.elements(s.structAxis.first), o)
				case _ => Nil
			}))

	def last(b: TableBlock, o: Orientation): Map[Axis, Int] = (
		Map.empty
		++ (b.axes(o).map(a => (a -> a.last)))
		++ (b match {
				case s: StructBlock if s.orientation == o =>
					last(s.elements(s.structAxis.last), o)
				case _ => Nil
			}))



}
