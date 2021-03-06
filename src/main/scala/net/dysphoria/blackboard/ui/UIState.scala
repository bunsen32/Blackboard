/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import scala.collection.mutable
import org.eclipse.swt.graphics.Point

import model._
import selection._

import org.eclipse.swt.widgets.Control

class UIState(val control: ViewCanvas) {
	var selectionHints: SelectionHints = NullSelectionHints
	private var originalSelection: Selectable = NullSelection
	private var currentSelection: Selectable = NullSelection
	var isIncludingNotExcluding = false
	var anchor: Selectable = NullSelection
	var focus: Selectable = NullSelection
	var dragState: DragObject = NoDrag
	private var _dropTarget: Option[Displayable] = None

	private var _selectLargeBits = false

	type StateChangedListener = Function[UIState, Unit]
	val stateChangedListeners = new mutable.HashSet[StateChangedListener]

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
			case _ if (focus != s) =>
				focus = s
				val range = makeRange(anchor, s)
				selection = combine(originalSelection, isIncludingNotExcluding, range)
			
			case _ =>
		}
	}

	def fineEditMode = control.cellEdit.visible
	def fineEditMode_=(on: Boolean) {
		if (on != fineEditMode) {
			if (on) 
				updateFineEditMode
			else
				control.cellEdit.endEdit
		}
	}

	def selection = currentSelection
	def selection_=(s: Selectable) {
		if (currentSelection != s) {
			currentSelection = s
			selectionHints = NullSelectionHints
			control.redraw
			if (fineEditMode) updateFineEditMode
			onChange
		}
	}

	def selectionHintsFor(orientation: Orientation) =
		selectionHints.forDirection(orientation, selection)

	private def updateFineEditMode {
		selection match {
			case cell: DataCellInstance =>
				control.cellEdit.beginEdit(new CellEditor(cell))

			case label: LabelInstance =>
				control.cellEdit.beginEdit(new LabelEditor(label))

			case _ => control.cellEdit.endEdit
		}
	}

	def selectLargeBits = _selectLargeBits
	def selectLargeBits_= (on: Boolean) {
		_selectLargeBits = on
		control.redraw
	}

	def dropTarget = _dropTarget
	def setDropTarget(d: Displayable) {
		_dropTarget = Some(d)
	}
	def removeDropTarget(d: Displayable){
		_dropTarget match {
			case Some(`d`) => _dropTarget = None
			case _ => ;//ignore
		}
	}

	def combine(base: Selectable, addDontRemove: Boolean, extension: Selectable): Selectable = {
		(base, extension) match {
			
/*			case (s: HasTableCells, other: HasTableCells) => {
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
*/
			case (NullSelection, _) =>
				if (addDontRemove) extension else NullSelection

			case (sel, NullSelection) => sel

			case _ => extension
		}
	}

	def makeRange(base: Selectable, extension: Selectable): Selectable = {
		(base, extension) match {
			case (s0: LabelInstance, s1: LabelInstance) => s0 to s1
/*
			case (s: HasTableCell, cell: HasTableCell) if cell.block == s.block =>
				s to cell

			case (s: HasDimensionLabel, l: HasDimensionLabel) if s.block == l.block => 
				s to l
*/
			case _ => NullSelection
		}
	}

	def onChange {
		for(ob <- stateChangedListeners) ob.apply(this)
	}

}

