/*
 * EditingStatePolicy.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import ui.selection._

/**
 * This class is responsible for deciding what operations are available in different
 * UI States, switching on and off overlays, and dispatching (high-level) operations.
 */
class EditingStatePolicy(control: ViewCanvas) extends Disposable {
	private val _selectionEditingOverlay = new SelectionEditingOverlay(control)
	private val device = control.getDisplay

	val insertRightSame = new InsertOverlayGlyph(device){val rotation = 0; val alt = false}
	val insertUpSame = new InsertOverlayGlyph(device){val rotation = 90; val alt = false}
	val insertLeftSame = new InsertOverlayGlyph(device){val rotation = 180; val alt = false}
	val insertDownSame = new InsertOverlayGlyph(device){val rotation = 270; val alt = false}

	val insertRightDiff = new InsertOverlayGlyph(device){val rotation = 0; val alt = true}
	val insertUpDiff = new InsertOverlayGlyph(device){val rotation = 90; val alt = true}
	val insertLeftDiff = new InsertOverlayGlyph(device){val rotation = 180; val alt = true}
	val insertDownDiff = new InsertOverlayGlyph(device){val rotation = 270; val alt = true}

	val singleCellCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, cellInsertSimilarRight _),
		EditingNodeSpec(Right, insertRightDiff, cellInsertDifferentRight _),
		EditingNodeSpec(Down, insertDownSame, cellInsertSimilarDown _),
		EditingNodeSpec(Down, insertDownDiff, cellInsertDifferentDown _))

	val singleColCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, labelInsertSimilarAfter _),
		EditingNodeSpec(Right, insertRightDiff, labelInsertDifferentAfter _))

	val singleRowCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, labelInsertSimilarAfter _),
		EditingNodeSpec(Down, insertDownDiff, labelInsertDifferentAfter _))

	control.ui.stateChangedListeners += (_ => updateState)

	def updateState {
		control.ui.selection match {
			case cell: CellSelection if editingSingleCellTable =>
				val rect = control.table.cellBounds(cell.coords)

			case lab: LabelSelection =>
				val rect = control.table.labelBounds(lab)
				_selectionEditingOverlay.set(rect, lab.orientation.choose(
					singleColCtrls,
					singleRowCtrls))

			case labs: LabelRangeSelection =>
				_selectionEditingOverlay.visible = false

			case _ =>
				// clear context buttons / selection-related operations
				_selectionEditingOverlay.visible = false
		}
		
	}

	def editingSingleCellTable = control.table.topBlock match {
			case a: ArrayBlock if a.xAxes.length == 0 && a.yAxes.length == 0 => true
			case _ => false
		}


	def labelInsertSimilarAfter {
		println("insertSimilarAfter")
	}

	def labelInsertDifferentAfter {
		println("insertDifferentAfter")
	}


	def cellInsertSimilarRight {
		println("cellInsertSimilarRight")
	}

	def cellInsertDifferentRight {
		println("cellInsertDifferentRight")
	}

	def cellInsertSimilarDown {
		println("cellInsertSimilarDown")
	}

	def cellInsertDifferentDown {
		println("cellInsertDifferentDown")
	}


	def dispose {
		insertRightSame.dispose
		insertUpSame.dispose
		insertLeftSame.dispose
		insertDownSame.dispose

		insertRightDiff.dispose
		insertUpDiff.dispose
		insertLeftDiff.dispose
		insertDownDiff.dispose
	}
}
