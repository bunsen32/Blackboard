/*
 * Navigator.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import selection._

abstract class Navigator {
	val topTable: Block

	def moveByCell(sel: CellSelection, o: Orientation, d: Int) = {
		def moveHelper(b: Block): Selectable = {
			b match {
				case s: StructBlock =>
					val el = s.elements(sel.coords(s.structAxis))
					moveHelper(el)

				case a: ArrayBlock =>
					val axes = a.axes(o)
					if (axes.isEmpty)
						NullSelection
					else {
						val axis = axes.last
						val p = sel.coords(axis)
						val max = axis.length
						val newP = p + d
						if (newP < 0 || newP > max)
							NullSelection
						else
							CellSelection(sel.coords + (axis -> newP))
					}
			}
		}
		moveHelper(topTable)
	}

}
