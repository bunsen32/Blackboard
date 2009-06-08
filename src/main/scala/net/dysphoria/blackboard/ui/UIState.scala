/*
 * Selector.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Point
import selection._

abstract sealed class DragState
case object NoDrag extends DragState
case object MouseDown extends DragState
case object Dragging extends DragState

class UIState(val control: GridView) {
	def grid = control.everything

	private var originalSelection: Selectable = NullSelection
	private var currentSelection: Selectable = NullSelection
	var isIncludingNotExcluding = false
	var anchor: Selectable = NullSelection
	var focus: Selectable = NullSelection
	/**
	 * Whether the mouse has moved far enough to register a 'drag' gesture
	 * (as opposed to merely a 'click').
	 */
	var dragState: DragState = NoDrag
	/**
	 * Point relative to the selection origin on which the user has clicked to drag.
	 */
	var dragAnchor: Option[Point] = None

	private var _selectLargeBits = false

	def select(s: Selectable) {
		originalSelection = s
		anchor = s
		focus = anchor
		isIncludingNotExcluding = true
		selection = originalSelection
	}

	def toggle(s: Selectable) = {
		isIncludingNotExcluding = ! selection.contains(s)
		originalSelection = combine(selection, isIncludingNotExcluding, s)
		anchor = s
		focus = anchor
		selection = originalSelection
	}

	def extendSelectionTo(s: Selectable) = {
		anchor match {
			case NullSelection => select(s)
			case _ => if (focus != s){
				focus = s
				val range = makeRange(anchor, s)
				selection = combine(originalSelection, isIncludingNotExcluding, range)
			}
		}
	}

	def selection = currentSelection
	def selection_=(s: Selectable) {
		currentSelection = s
		control.redraw
	}

	def selectLargeBits = _selectLargeBits
	def selectLargeBits_= (on: Boolean) {
		_selectLargeBits = on
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

