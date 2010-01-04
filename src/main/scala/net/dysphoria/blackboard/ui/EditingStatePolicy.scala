/*
 * EditingStatePolicy.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import net.dysphoria.blackboard._
import actions.InapplicableActionException
import selection._

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

	val deleteGlyph = new DeleteOverlayGlyph(device)

	val singleCellCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, ()=>cellInsertRepeatedAfter(Horizontal)),
		EditingNodeSpec(Right, insertRightDiff, ()=>cellInsertDifferentAfter(Horizontal)),
		EditingNodeSpec(Down, insertDownSame, ()=>cellInsertRepeatedAfter(Vertical)),
		EditingNodeSpec(Down, insertDownDiff, ()=>cellInsertDifferentAfter(Vertical)))

	val singleCellXCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, ()=>cellInsertRepeatedAfter(Horizontal)),
		EditingNodeSpec(Right, insertRightDiff, ()=>cellInsertDifferentAfter(Horizontal)))

	val singleCellYCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, ()=>cellInsertRepeatedAfter(Vertical)),
		EditingNodeSpec(Down, insertDownDiff, ()=>cellInsertDifferentAfter(Vertical)))

	val labelColRepCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, labelInsertRepeatedAfter _),
		EditingNodeSpec(Right, insertRightDiff, labelInsertDifferentAfter _))

	val labelRowRepCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, labelInsertRepeatedAfter _),
		EditingNodeSpec(Down, insertDownDiff, labelInsertDifferentAfter _))

	val labelColSamCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, labelInsertSimilarAfter _),
		EditingNodeSpec(Right, insertRightDiff, labelInsertDifferentAfter _))

	val labelRowSamCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, labelInsertSimilarAfter _),
		EditingNodeSpec(Down, insertDownDiff, labelInsertDifferentAfter _))

	val labelColArrayCtrls = Seq(
		EditingNodeSpec(Right, insertRightSame, labelInsertSimilarAfter _))

	val labelRowArrayCtrls = Seq(
		EditingNodeSpec(Down, insertDownSame, labelInsertSimilarAfter _))

	val deleteColAxisCtrls = Seq(
		EditingNodeSpec(Left, deleteGlyph, labelDeleteAxis _))

	val deleteRowAxisCtrls = Seq(
		EditingNodeSpec(Up, deleteGlyph, labelDeleteAxis _))

	val deleteColLabelCtrls = Seq(
		EditingNodeSpec(Up, deleteGlyph, labelDeleteData _))

	val deleteRowLabelCtrls = Seq(
		EditingNodeSpec(Left, deleteGlyph, labelDeleteData _))

	control.ui.stateChangedListeners += (_ => updateState)

	def updateState {
		def labelCtrls(o: Orientation, deleteLabel: Boolean, deleteAxis: Boolean, num: Int) = {
			(num match {
				case 0 => Nil
				case 1 => o.choose(labelColArrayCtrls, labelRowArrayCtrls)
				case 2 => o.choose(labelColSamCtrls, labelRowSamCtrls)
				case 3 => o.choose(labelColRepCtrls, labelRowRepCtrls)
			}) ++ (
				if (deleteAxis) o.choose(deleteColAxisCtrls, deleteRowAxisCtrls) else Nil
			) ++ (
				if (deleteLabel) o.choose(deleteColLabelCtrls, deleteRowLabelCtrls) else Nil
			)
		}

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
				val axis = lab.axis
				val deleteAxis = axis.length <= 1
				val deleteLabel = (axis.length - 1 >= axis.minimumLength)
				_selectionEditingOverlay.set(rect, labelCtrls(lab.orientation, deleteLabel, deleteAxis, axis match {
					case s: StructAxis => 3
					case a: ArrayAxis if lab.index >= a.last => 2
					case _ => 1
				}))

			case labs: LabelRange =>
				val rect = labelRangeRect(control.table, labs)
				val axis = labs.axis
				val deleteAxis = axis.length <= labs.range.length
				val deleteLabel = (axis.length - labs.range.length) >= axis.minimumLength
				_selectionEditingOverlay.set(rect, labelCtrls(labs.orientation, deleteLabel, deleteAxis, axis match {
					case s: StructAxis => 3
					case a: ArrayAxis if eq(labs.range, a.range) => 3
					case a: ArrayAxis => 0
					case _ => 0
				}))

			case _ =>
				// clear context buttons / selection-related operations
				_selectionEditingOverlay.visible = false
		}
		
	}

	/*------------------------------------------------------------------------*/
	// ACTIONS

	def labelInsertRepeatedAfter {
		def repeatWholeAxis(labs: LabelSelection) {
			val block = labs.block
			val existingAxes = block.axes(labs.orientation)
			val newAxis = new ArrayAxis(2){interItemLine = thickerThan(labs.axis.interItemLine)}
			block.axes(labs.orientation) = existingAxes.take(labs.axisIndex) ++ Some(newAxis) ++ existingAxes.drop(labs.axisIndex)
			forAllArraysInBlock(block, _.addDimension(newAxis))

			control.ui.selection = NullSelection
		}

		control.ui.selection match {
			case lab: OneLabel =>
				lab.axis match {
					case a: ArrayAxis =>
						validActionIf(lab.axis.length == 1)
						repeatWholeAxis(lab)

					case s: StructAxis =>
						// Repeat the single struct item
						val b = lab.block.asInstanceOf[StructBlock]
						val el = b.elementFor(lab.coords)
						val newAxis = new ArrayAxis(2){interItemLine = thinLine}
						el.axes(lab.orientation) = Seq(newAxis) ++ el.axes(lab.orientation)
						forAllArraysInBlock(el, _.addDimension(newAxis))
				}
			case labs: LabelRange =>
				// If user selected range of labels, assume we’re to repeat that range.
				labs.axis match {
					case ax if (eq(ax.range, labs.range)) => repeatWholeAxis(labs)

					case a: ArrayAxis =>
						assert(!eq(a.range, labs.range))
						notValidAction

					case oldAxis: StructAxis =>
						// If user selects a range of struct items, and chooses to
						// ‘insert similar after’, we treat that as repeating only these struct items.
						assert(!eq(oldAxis.range, labs.range)) // Already dealt with that above => must genuinely split axis
						val newLab = groupStructLabels(labs)
						val newBlock = newLab.block.asInstanceOf[StructBlock].elementFor(newLab.coords)

						val newDim = new ArrayAxis(2){interItemLine = thinnerThan(oldAxis.interItemLine)}
						forAllArraysInBlock(newBlock, _.addDimension(newDim))
						newBlock.axes(labs.orientation) = Seq(newDim) ++ newBlock.axes(labs.orientation)

						control.ui.selection = newLab
				}
			case _ => notValidAction
		}
		updateDisplay
	}

	/**
	 * Replace given range of Struct labels with a single label which itself contains
	 * all of the specified labels. Returns a OneLabel which is the new, grouped label.
	 */
	def groupStructLabels(labs: LabelRange) = (labs.block, labs.axis) match {
		case (oldBlock: StructBlock, oldAxis: StructAxis) =>
			val (startIx, endIx) = (labs.range.start, labs.range.last + 1)

			val newAxis = new StructAxis(labs.range.length){interItemLine = thinnerThan(thinnerThan(oldAxis.interItemLine))}
			val newBlock = new StructBlock(labs.table, newAxis)
			newBlock.orientation = labs.orientation
			newBlock.elements ++= oldBlock.elements.slice(startIx, endIx)
			newBlock.axes(labs.orientation) = Seq(newAxis)
			assert(newBlock.elements.length == newAxis.length)

			for(_ <- labs.range) {
				oldBlock.elements.remove(startIx)
				oldAxis.delete(startIx, 1)
			}
			oldBlock.elements.insert(startIx, newBlock)
			oldAxis.insert(startIx, 1)
			assert(oldBlock.elements.length == oldAxis.length)
			labs.first

		case _ => throw new IllegalArgumentException("groupStructLabels: requires Struct axis/block")
	}

	def labelInsertSimilarAfter {
		control.ui.selection match {
			case lab: OneLabel =>
				lab.axis match {
					case a: ArrayAxis =>
						val i = if (lab.index >= lab.axis.length) lab.index else lab.index + 1
						a.insert(i, 1)
						updateDisplay

					case s: StructAxis => notValidAction
				}
			case _ => notValidAction
		}
	}


	/**
	 * Insert a ‘different’ column (implies a struct). If added to existing
	 * struct axis, just insert a new value in that axis; if added to (the end of)
	 * an existing array axis, adds a whole new StructAxis above the ArrayAxis.
	 */
	def labelInsertDifferentAfter {
		control.ui.selection match {
			case lab: OneLabel => insertDifferentAfterSingleLabel(lab)
			case labs: LabelRange => insertDifferentAfterSingleLabel(labs.last)
			case _ => notValidAction
		}
	}

	def insertDifferentAfterSingleLabel(lab: OneLabel) {
		(lab.axis, lab.block) match {
			case (s: StructAxis, b: StructBlock) =>
				s.insert(lab.index + 1, 1)
				val arr = new ArrayBlock(lab.table)
				for((ax: ArrayAxis, _) <- lab.parentCoords) arr.array.addDimension(ax)
				for(a <- lab.block.xAxes) onlyArrayAxis(a, arr.array.addDimension(_))
				for(a <- lab.block.yAxes) onlyArrayAxis(a, arr.array.addDimension(_))
				b.elements.insert(lab.index + 1, arr)
				updateDisplay

			case (a: ArrayAxis, b) =>
				val existingAxes = b.axes(lab.orientation)
				val structAxisIndex = existingAxes.findIndexOf(_.isInstanceOf[StructAxis])
				// Currently a struct axes is always the ‘lowest’ (latest) axis
				// on a StructBlock’s primary orientation... so adding a struct axis
				// above an array axis implies adding it above any existing other
				// struct axis. [If we want to handle case where adding new struct
				// axis below existing one, need a) to determine correct semantics
				// and b) implement it!]
				validActionIf(structAxisIndex == -1 || structAxisIndex > lab.axisIndex)
				control.table.topBlock = splitAfterArray(control.table.topBlock, lab, a)
				
				control.ui.selection = NullSelection
				updateDisplay
		}
	}

	def splitAfterArray(fromBlock: TableBlock, lab: OneLabel, arrayAxis: ArrayAxis): TableBlock = {
		fromBlock match {
			case s: StructBlock if s != lab.block =>
				val i = lab.coords(s.structAxis)
				s.elements(i) = splitAfterArray(s.elements(i), lab, arrayAxis)
				s

			case b if b == lab.block =>
				val (o, orthogonal) = (lab.orientation, lab.orientation.opposite)
				val (promotedAxes, existingAxes) = splitAt(b.axes(lab.orientation), lab.axisIndex)
				val (promotedOrthAxes, existingOrthAxes) = topLevelArrayAxes(b.axes(orthogonal))
				
				assert(b match {case s: StructBlock => s.orientation != o || existingAxes.contains(s.structAxis); case _ => true})
				b.axes(o) = existingAxes
				b.axes(orthogonal) = existingOrthAxes

				val newChild = new ArrayBlock(fromBlock.table)
				for(ax <- promotedAxes) onlyArrayAxis(ax, newChild.array.addDimension(_))
				for(ax <- promotedOrthAxes) onlyArrayAxis(ax, newChild.array.addDimension(_))
				for(ax <- lab.parentCoords.keys) onlyArrayAxis(ax, newChild.array.addDimension(_))

				val newAxis = new StructAxis(2){interItemLine = thickerThan(arrayAxis.interItemLine)}
				newAxis.elements(0) = ("data", false) // Hide struct element for existing array.
				val newTop = new StructBlock(fromBlock.table, newAxis)
				newTop.orientation = o
				newTop.axes(o) = promotedAxes ++ Some(newAxis)
				newTop.axes(orthogonal) = promotedOrthAxes
				newTop.elements ++= Seq(b, newChild)

				newTop

			case _ => error("fromBlock must be either a StructBlock, or else the target block.")
		}
	}


	def cellInsertRepeatedAfter(o: Orientation) {
		validActionIf(topBlock.axes(o).length == 0)

		val newAxis = new ArrayAxis(1){interItemLine = thinLine}
		newAxis.insert(1, 1) // A new column
		topBlock.axes(o) = List(newAxis)
		forAllArraysInBlock(topBlock, a => a.addDimension(newAxis))

		control.ui.selection = NullSelection
		updateDisplay
	}

	def cellInsertDifferentAfter(o: Orientation) {
		validActionIf(topBlock.isInstanceOf[ArrayBlock] && topBlock.axes(o).length == 0)

		val orthogonal = o.opposite
		val(promotedArrayAxes, existingOrthAxes) = topLevelArrayAxes(topBlock.axes(orthogonal))

		topBlock.axes(orthogonal) = existingOrthAxes
		val newChild = new ArrayBlock(topBlock.table)
		newChild.axes(orthogonal) = existingOrthAxes
		for(ax <- promotedArrayAxes) newChild.array.addDimension(ax)

		val newAxis = new StructAxis(2){interItemLine = thinLine}
		val newTop = new StructBlock(topBlock.table, newAxis)
		newTop.elements ++= Seq(topBlock, newChild)
		newTop.orientation = o
		newTop.axes(o) = List(newAxis)
		newTop.axes(orthogonal) = promotedArrayAxes

		control.table.topBlock = newTop

		control.ui.selection = NullSelection
		updateDisplay
	}

	
	def labelDeleteData {
		def delete(block: TableBlock, axis: Axis, i: Int, count: Int) {
			validActionIf((axis.length - count) >= axis.minimumLength)
			(axis, block) match {
				case (a: ArrayAxis, _) => a.delete(i, count)
				case (s: StructAxis, sb: StructBlock) =>
					s.delete(i, count)
					for(_ <- 1 to count) sb.elements.remove(i)
			}
			control.ui.selection = NullSelection
			updateDisplay
		}

		control.ui.selection match {
			case lab: OneLabel => delete(lab.block, lab.axis, lab.index, 1)
			case labs: LabelRange => delete(labs.block, labs.axis, labs.range.start, labs.range.length)
			case _ => notValidAction
		}
	}

	def labelDeleteAxis {
		control.ui.selection match {
			case sel: LabelSelection =>
				val block = sel.block
				val o = sel.orientation
				sel.axis match {
					case a: ArrayAxis =>
						forAllArraysInBlock(block, _.removeDimension(a))
						block.axes(o) = block.axes(o).filter(_ != a)
						
					case s: StructAxis =>
						control.table.topBlock = coalesceStruct(control.table.topBlock, sel)
				}
				control.ui.selection = NullSelection
				updateDisplay

			case _ => notValidAction
		}
	}

	def coalesceStruct(block: TableBlock, sel: LabelSelection): TableBlock = {
		block match {
			case s: StructBlock if s != sel.block =>
				val i = sel.parentCoords(s.structAxis)
				s.elements(i) = coalesceStruct(s.elements(i), sel)
				s

			case s: StructBlock if s == sel.block =>
				val child = s.elements(0)
				def joinAxes(o: Orientation) {
					child.axes(o) = s.axes(o).filter(_ != sel.axis) ++
									child.axes(o)
				}
				joinAxes(Horizontal)
				joinAxes(Vertical)
				// Should need to do nothing more to dispose of ‘s’. Potentially in the
				// future we need to initiate some kind of clean-up.
				child

			case _ => error("‘block’ must be sel.block or one of its ancestors.")
		}
	}


	/*------------------------------------------------------------------------*/
	// GENERAL UTILITY

	private def forAllArraysInBlock(b: TableBlock, f: FlexibleArrayTable=>Unit): Unit =
		b match {
			case a: ArrayBlock => f(a.array) // TODO
			case b: StructBlock =>
				for(el <- b.elements)
					forAllArraysInBlock(el, f)
		}

	private def splitAt[A](seq: Seq[A], index: Int): (Seq[A], Seq[A]) =
		(seq.take(index), seq.drop(index))

	
	private def topLevelArrayAxes(axes: Seq[Axis]): (Seq[ArrayAxis], Seq[Axis]) =
		// Yeah, ugly implementation
		(axes.takeWhile(a => a.isInstanceOf[ArrayAxis]).asInstanceOf[Seq[ArrayAxis]],
		 axes.dropWhile(a => a.isInstanceOf[ArrayAxis]))


	private def onlyArrayAxis(o: Axis, f: ArrayAxis=>Unit) =
		if (o.isInstanceOf[ArrayAxis]) f(o.asInstanceOf[ArrayAxis])

	private def labelRangeRect(t: Table, labs: LabelRange) = {
		val r1 = t.labelBounds(labs.first)
		val r2 = t.labelBounds(labs.last)
		r1.union(r2)
	}

	// Required for now (pre Scala2.8), since Range equality is broken in 2.7
	private def eq(r1: Range, r2: Range) =
		(r1.start == r2.start) && (r1.last == r2.last) && (r1.step == r2.step)


	private def validActionIf(p: Boolean) =
		if (!p) notValidAction

	private def notValidAction = throw new InapplicableActionException

	// TODO: Eventually this should not be required; changes to model should
	// automatically update the view.
	def updateDisplay {
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

	/*------------------------------------------------------------------------*/
	// LINES

	import gfx.LineDescriptor
	import org.eclipse.swt.graphics.RGB

	val thinLine = Some(new LineDescriptor(new RGB(0, 0, 100), 25))
	def thickerThan(l: Option[LineDescriptor]) = l match {
		case None => thinLine
		case Some(line) => Some(new LineDescriptor(line.colour, line.thickness * 2F))
	}
	def thinnerThan(l: Option[LineDescriptor]) = l match {
		case None => thinLine
		case Some(line) => Some(new LineDescriptor(line.colour, line.thickness * 0.75F))
	}
}
