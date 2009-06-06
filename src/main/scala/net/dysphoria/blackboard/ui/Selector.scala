/*
 * Selector.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import selection._

class Selector(val control: GridView) {
	def grid = control.everything

	var originalSelection: Selectable = NullSelection
	var isIncludingNotExcluding = false
	var anchor: Option[Selectable] = None
	var focus: Option[Selectable] = None

	def select(s: Selectable) {
		originalSelection = s
		anchor = s match {case NullSelection => None; case _ => Some(s)}
		focus = anchor
		isIncludingNotExcluding = true
		selection = originalSelection
	}

	def toggle(s: Selectable) = {
		isIncludingNotExcluding = ! selection.contains(s)
		originalSelection = combine(selection, isIncludingNotExcluding, s)
		anchor = Some(s)
		focus = anchor
		selection = originalSelection
	}

	def extendTo(s: Selectable) = {
		anchor match {
			case None => select(s)
			case Some(anchorPoint) => {
				focus = Some(s)
				val range = makeRange(anchorPoint, s)
				selection = combine(originalSelection, isIncludingNotExcluding, range)
			}
		}
	}

	def selection = grid.selection
	def selection_=(s: Selectable) {
		grid.selection = s
		control.redraw
	}


	def combine(base: Selectable, addDontRemove: Boolean, extension: Selectable): Selectable = {
		(base, extension) match {
			
			case (s: HasTableCells, other: HasTableCells) => {
				val s1 = s.toCellSet
				val s2 = other.toCellSet
				if (addDontRemove) s1 + s2 else s1 - s2
			}

			case (s: HasDimensionLabels, other: HasDimensionLabels) => {
				val s1 = s.toLabelSet
				val s2 = other.toLabelSet
				val result = if (addDontRemove) s1 + s2 else s1 - s2
				result
			}

			case (NullSelection, _) =>
				if (addDontRemove) extension else NullSelection

			case (sel, NullSelection) => sel

			case _ => extension
		}
	}

	def makeRange(base: Selectable, extension: Selectable): Selectable = {
		(base, extension) match {
			
			case (s: HasTableCell, cell: HasTableCell) if cell.block == s.block =>
				s to cell

			case (s: HasDimensionLabel, l: HasDimensionLabel) if s.block == l.block => 
				s to l

			case _ => NullSelection
		}
	}

}

