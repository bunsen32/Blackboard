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
		EditingNodeSpec(Right, insertRightSame, ()=>cellInsertSimilarAfter(Horizontal)),
		EditingNodeSpec(Right, insertRightDiff, ()=>cellInsertDifferentAfter(Horizontal)),
		EditingNodeSpec(Down, insertDownSame, ()=>cellInsertSimilarAfter(Vertical)),
		EditingNodeSpec(Down, insertDownDiff, ()=>cellInsertDifferentAfter(Vertical)))

	val singleCellXCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, ()=>cellInsertSimilarAfter(Horizontal)),
		EditingNodeSpec(Right, insertRightDiff, ()=>cellInsertDifferentAfter(Horizontal)))

	val singleCellYCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, ()=>cellInsertSimilarAfter(Vertical)),
		EditingNodeSpec(Down, insertDownDiff, ()=>cellInsertDifferentAfter(Vertical)))

	val labelColCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, labelInsertSimilarAfter _),
		EditingNodeSpec(Right, insertRightDiff, labelInsertDifferentAfter _))

	val labelRowCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, labelInsertSimilarAfter _),
		EditingNodeSpec(Down, insertDownDiff, labelInsertDifferentAfter _))

	val labelColArrayCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, labelInsertSimilarAfter _))

	val labelRowArrayCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, labelInsertSimilarAfter _))

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

			case lab: OneLabel =>
				val rect = control.table.labelBounds(lab)
				val allowedDiff = lab.axis match {
					case s: StructAxis => true
					case a: ArrayAxis if lab.index == a.last => true
					case _ => false
				}
				val ctrls = if (allowedDiff)
						lab.orientation.choose(labelColCtrls, labelRowCtrls)
					else
						lab.orientation.choose(labelColArrayCtrls, labelRowArrayCtrls)
						
				_selectionEditingOverlay.set(rect, ctrls)

			case labs: LabelRange =>
				_selectionEditingOverlay.visible = false

			case _ =>
				// clear context buttons / selection-related operations
				_selectionEditingOverlay.visible = false
		}
		
	}


	def labelInsertSimilarAfter {
		control.ui.selection match {
			case lab: OneLabel =>
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
		def onlyArrayAxis(ax: Axis, f: ArrayAxis=>Unit) = ax match {
			case a: ArrayAxis => f(a)
			case _ => //ignore
		}

		control.ui.selection match {
			case lab: OneLabel =>
				(lab.axis, lab.block) match {
					case (s: StructAxis, b: StructBlock) =>
						s.insert(lab.index + 1)
						val arr = new ArrayBlock
						for((ax: ArrayAxis, _) <- lab.parentCoords) arr.array.addDimension(ax)
						for(a <- lab.block.xAxes) onlyArrayAxis(a, arr.array.addDimension(_))
						for(a <- lab.block.yAxes) onlyArrayAxis(a, arr.array.addDimension(_))
						b.elements.insert(lab.index + 1, arr)
						updateDisplay

					case (a: ArrayAxis, b) =>
						// TODO: make this work.
				}
			case _ => //ignore
		}
	}


	def cellInsertSimilarAfter(o: Orientation) {
		// TODO: should be a check (and fail if false) rather than an assertion.
		assert(topBlock.axes(o).length == 0)

		val newAxis = new ArrayAxis(1)
		newAxis.insert(1) // A new column
		topBlock.axes(o) = List(newAxis)
		forAllArraysInBlock(topBlock, a => a.addDimension(newAxis))
		updateDisplay
	}

	def cellInsertDifferentAfter(o: Orientation) {
		// TODO: should be a check (and fail if false) rather than an assertion.
		assert(topBlock.axes(o).length == 0)

		val orthogonal = o.opposite
		val(promotedArrayAxes, existingOrthAxes) = topLevelArrayAxes(topBlock.axes(orthogonal))

		val newChild = new ArrayBlock
		newChild.axes(orthogonal) = promotedArrayAxes
		for(ax <- promotedArrayAxes) newChild.array.addDimension(ax)

		val newAxis = new StructAxis(2)
		val newTop = new StructBlock(newAxis)
		newTop.orientation = o
		newTop.axes(o) = List(newAxis)
		newTop.axes(orthogonal) = promotedArrayAxes
		newTop.elements ++= Seq(topBlock, newChild)

		topBlock.axes(orthogonal) = existingOrthAxes
		control.table.topBlock = newTop

		control.ui.selection = NullSelection
		updateDisplay
	}

	private def forAllArraysInBlock(b: TableBlock, f: FlexibleArrayTable=>Unit): Unit =
		b match {
			case a: ArrayBlock => f(a.array) // TODO
			case b: StructBlock =>
				for(el <- b.elements)
					forAllArraysInBlock(el, f)
		}


	private def topLevelArrayAxes(axes: Seq[Axis]): (Seq[ArrayAxis], Seq[Axis]) =
		// Yeah, ugly implementation
		(axes.takeWhile(a => a.isInstanceOf[ArrayAxis]).asInstanceOf[Seq[ArrayAxis]],
		 axes.dropWhile(a => a.isInstanceOf[ArrayAxis]))


	// TODO: This should not be required; changes to model should automatically
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
