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

	val singleCellXCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, cellInsertSimilarRight _),
		EditingNodeSpec(Right, insertRightDiff, cellInsertDifferentRight _))

	val singleCellYCtrls = Seq(
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
		val noXAxes = (topBlock.xAxes.length == 0)
		val noYAxes = (topBlock.yAxes.length == 0)
		control.ui.selection match {
			case cell: CellSelection if noXAxes || noYAxes =>
				val rect = control.table.cellBounds(cell.coords)
				val ctrls = if (noXAxes && noYAxes)
						singleCellCtrls
					else if (noXAxes)
						singleCellXCtrls
					else
						singleCellYCtrls
				_selectionEditingOverlay.set(rect, ctrls)

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


	def labelInsertSimilarAfter {
		control.ui.selection match {
			case lab: LabelSelection =>
				lab.axis match {
					case a: ArrayAxis =>
						a.insert(lab.index + 1)
						updateDisplay

					case s: StructAxis =>
						// TODO: make this work.
				}
			case _ => //ignore
		}
	}

	def labelInsertDifferentAfter {
		println("insertDifferentAfter")
	}


	def cellInsertSimilarRight {
		// TODO: should be a check (and fail if false) rather than an assertion.
		assert(topBlock.xAxes.length == 0)

		val newAxis = new ArrayAxis(1)
		newAxis.insert(1) // A new column
		topBlock.xAxes = List(newAxis)
		forAllArraysInBlock(topBlock, a => a.addDimension(newAxis))
		updateDisplay
	}

	def cellInsertDifferentRight {
		println("cellInsertDifferentRight")
	}

	def cellInsertSimilarDown {
		// TODO: should be a check (and fail if false) rather than an assertion.
		assert(topBlock.yAxes.length == 0)

		val newAxis = new ArrayAxis(1)
		newAxis.insert(1) // A new column
		topBlock.yAxes = List(newAxis)
		forAllArraysInBlock(topBlock, a => a.addDimension(newAxis))
		updateDisplay
	}

	def cellInsertDifferentDown {
		println("cellInsertDifferentDown")
	}

	private def forAllArraysInBlock(b: TableBlock, f: FlexibleArrayTable=>Unit): Unit =
		b match {
			case a: ArrayBlock => f(a.array) // TODO
			case b: StructBlock =>
				for(el <- b.elements)
					forAllArraysInBlock(el, f)
		}


	// TODO: This should not be required; changed to model should automatically
	// update the view.
	private def updateDisplay {
		control.table.computeSize
		control.computeBounds
		control.redraw
		updateState
	}

	def topBlock = control.table.topBlock

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
