/*
 * BlockDragClient.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import org.eclipse.swt.widgets.Control
import selection._
import gfx._

class BlockDragClient(control: GridView) extends GridViewDragClient(control) {
	import control.ui
	val grid = control.everything
	val selection = ui.selection match {
		case b: SingleGridSpace => b
		case _ => error("Block drag requires a selection which is a block")
	}
	val _block = selection.block
	var _currentTargetControl: Option[GridView] = None
	var _dropTarget: DropTarget = NoDropTarget
	var _pivotPoints: Set[DisplayBlock] = Set.empty
	var _pivotColumns: Set[Int] = Set.empty
	var _pivotRows: Set[Int] = Set.empty

	override def start(op: DragOperation, m: MouseDown) {
		// TODO: we're dragging the 'selection', but the MouseDown(x,y) is relative
		// to the current focus object (Which should nonetheless be /part of/ the
		// selection.)
		ui.dragState = Dragging
		op.size = _block.size
		op.dragAnchor = new Point(m.x, m.y)
		identifyPivotPoints
	}

	private def identifyPivotPoints = _block match {
		case labs: DimensionLabelsBlock =>
				allPivotsFrom(labs.displayDimension,
							  if (labs.isXNotY) grid.column(_block.xIndex) else grid.row(_block.yIndex))
		case _ => {
			_pivotPoints = Set.empty
			_pivotColumns = Set.empty
			_pivotRows = Set.empty
		}
	}

	private def allPivotsFrom(dd: DisplayDimension, initialCandidates: Iterable[DisplayBlock]) {
		
		def transitiveClosurePivotPoints(searchedXes: Set[Int], searchedYs: Set[Int], foundPivots: Set[DisplayBlock], candidates: Iterable[DisplayBlock]) {
			val newPivots = candidates filter (b => !foundPivots.contains(b) && isPivot(b))
			val lazyNewPivots = newPivots.projection
			val newXes = Set.empty ++ (lazyNewPivots.map(b => b.xIndex).filter(!searchedXes.contains(_)))
			val newYs  = Set.empty ++ (lazyNewPivots.map(b => b.yIndex).filter(!searchedYs.contains(_)))
			if (newPivots.isEmpty && newXes.isEmpty && newYs.isEmpty) {
				_pivotPoints = foundPivots
				_pivotColumns = searchedXes
				_pivotRows = searchedYs
			
			}else {
				val newCandidates = (newXes flatMap (grid.column _)) ++ (newYs flatMap (grid.row _))
				transitiveClosurePivotPoints(searchedXes++newXes, searchedYs++newYs, foundPivots++newPivots, newCandidates)
			}
		}

		def isPivot(block: DisplayBlock): Boolean = block match {
			case _: DimensionLabelsBlock => false // We ignore (other) DimensionLabels
			case t: TableBlock if t.uses(dd) => true
			case _ => false
		}

		transitiveClosurePivotPoints(Set.empty, Set.empty, Set.empty, initialCandidates)
	}

	override def paint(gc: GC) {
		_block.render(new DrawingContext(gc, ui), new Point(0, 0))
	}
	override def move(destControl: Control, offset: Point) {
		if (destControl == control) {
			if (currentTargetControl == None || currentTargetControl.get != control)
				currentTargetControl = Some(control)
				
			val modelOffset = control.viewToModel(offset)
			val (xGap, dx) = grid.xDimensionLists.gapHitTest(modelOffset.x)
			val (yGap, dy) = grid.yDimensionLists.gapHitTest(modelOffset.y)
			val ix = if (dx < 0) xGap - 1 else xGap // -1 to max(horiz) + 1
			val iy = if (dy < 0) yGap - 1 else yGap // -1 to max(vert) + 1
			val destX = if (selection.ix < xGap) xGap - 1 else xGap
			val destY = if (selection.iy < yGap) yGap - 1 else yGap
			val srcX = selection.ix
			val srcY = selection.iy
			// Whether we're closer to a horizontal gap than a vertical one.
			val preferHoriz = Math.abs(dx) > Math.abs(dy)

			def rotationDropTarget: DropTarget = _block match {
				case block: DimensionLabelsBlock => {
					if (block.isXNotY)
						if (iy >=0 && iy < grid.yDimensionLists.length &&
							(ix != srcX || !preferHoriz) && iy != srcY &&
							_pivotRows.contains(iy)) {
							DimensionRotatedVertical(xGap)

						} else
							NoDropTarget
							
					else
						if (ix >= 0 && ix < grid.xDimensionLists.length &&
							(iy != srcY || preferHoriz) && ix != srcX &&
							_pivotColumns.contains(ix)) {
							DimensionRotatedHorizontal(yGap)
						
						}else
							NoDropTarget
				}
				case _ => NoDropTarget
			}
			
			dropTarget =
				if (ix == srcX && iy == srcY)
					NoDropTarget
				
				else rotationDropTarget match {
					case NoDropTarget => {
						if (ix == srcX) // Same column
							if (destY != srcY) RowMovedTo(yGap) else NoDropTarget

						else if (iy == srcY) // Same row
							if (destX != srcX) ColumnMovedTo(xGap) else NoDropTarget

						else NoDropTarget
					}
					case rotation => rotation
				}

		}else{
			currentTargetControl = None
			dropTarget = NoDropTarget
		}
	}
	override def stop {
		currentTargetControl = None
	}

	def currentTargetControl = _currentTargetControl
	def currentTargetControl_=(newTarget: Option[GridView]) {
		if (newTarget != _currentTargetControl) {
			if (_currentTargetControl.isDefined){
				_currentTargetControl.get.redraw
				_currentTargetControl.get.ui.removeDropTarget(DropTargetRenderer)
			}
			_currentTargetControl = newTarget
			if (_currentTargetControl.isDefined)
				_currentTargetControl.get.ui.setDropTarget(DropTargetRenderer)
		}
	}
	def dropTarget = _dropTarget
	def dropTarget_=(newDropTarget: DropTarget){
		if (_dropTarget != newDropTarget){
			_dropTarget = newDropTarget
			control.redraw
			dragOperation.rotationRadians = newDropTarget match {
				case _: DimensionRotatedHorizontal |
					_:DimensionRotatedVertical => (Math.Pi/2D).toFloat
				case _ => 0F
			}
		}
	}

	abstract sealed class DropTarget
	case object NoDropTarget extends DropTarget
	case class ColumnMovedTo(xGap: Int) extends DropTarget
	case class RowMovedTo(yGap: Int) extends DropTarget
	case class DimensionRotatedHorizontal(yGap: Int) extends DropTarget
	case class DimensionRotatedVertical(xGap: Int) extends DropTarget

	object DropTargetRenderer extends Displayable {
		override def render(gfx: DrawingContext, origin: Point) {

			def drawLine(x1: Int, y1: Int, x2: Int, y2: Int) {
				Style.DropInsertionLine.setAttributesOf(gfx)
				var logical = new Point(x1, y1)
				val p1 = control.modelToView(logical)
				logical.x = x2; logical.y = y2; // Ugly, but avoid creating another object
				val p2 = control.modelToView(logical)
				gfx.gc.drawLine(p1.x, p1.y, p2.x, p2.y)
			}

			def drawPivotPoints {
				gfx.gc.setBackground(gfx.gc.getDevice.getSystemColor(SWT.COLOR_RED))
				gfx.gc.setAlpha(128)
				for(block <- _pivotPoints){
					val p0 = control.modelToView(
						grid.xDimensionLists.gapPosition(block.xIndex),
						grid.yDimensionLists.gapPosition(block.yIndex))
				  
					val p1 = control.modelToView(
						grid.xDimensionLists.gapPosition(block.xIndex+1),
						grid.yDimensionLists.gapPosition(block.yIndex+1))
				  
					gfx.gc.fillRectangle(p0.x, p0.y, p1.x-p0.x, p1.y-p0.y)
				}
			}

			dropTarget match {
				case NoDropTarget => ;// Nothing to draw
				case ColumnMovedTo(xGap) => {
					val x = grid.xDimensionLists.gapPosition(xGap)
					val h = grid.size.y
					drawLine(x, 0, x, h)
				}
				case RowMovedTo(yGap) => {
					val y = grid.yDimensionLists.gapPosition(yGap)
					val w = grid.size.x
					drawLine(0, y, w, y)
				}
				case DimensionRotatedHorizontal(yGap) => {
					for(xCol <- _pivotColumns){
						val x = grid.xDimensionLists.gapPosition(xCol)
						val y = grid.yDimensionLists.gapPosition(yGap)
						val w = DisplayDimension.widthOf(grid.xDimensionLists(xCol))
						drawLine(x, y, x + w, y)
					}
					drawPivotPoints
				}
				case DimensionRotatedVertical(xGap) => {
					for(yRow <- _pivotRows){
						val x = grid.xDimensionLists.gapPosition(xGap)
						val y = grid.yDimensionLists.gapPosition(yRow)
						val h = DisplayDimension.widthOf(grid.yDimensionLists(yRow))
						drawLine(x, y, x, y + h)
					}
					drawPivotPoints
				}
			}
		}
		override def size = control.getSize
	}

	override def commit {
		val ix = selection.ix
		val iy = selection.iy
		dropTarget match {
			case NoDropTarget => ;// Nothing to draw
			case ColumnMovedTo(xGap) => {
				val destX = if (ix < xGap) xGap - 1 else xGap
				val dims = grid.xDimensionLists(ix)
				val col = grid.column(ix).toArray
				val oldWidth = grid.xGridSize
				grid.deleteCol(ix)
				grid.insertCol(destX)
				assert(dims != Nil)
				grid.xDimensionLists(destX) = dims
				pasteColumn(destX, col)
				_block match {
					case labels: DimensionLabelsBlock => {
						val r = grid.row(iy)
						grid.yDimensionLists(iy) = grid.reorderDisplayDimensions(r)
					}
					case _=>;//ignore
				}
			}
			case RowMovedTo(yGap) => {
				val destY = if (iy < yGap) yGap - 1 else yGap
				val dims = grid.yDimensionLists(iy)
				val r = grid.row(iy).toArray
				grid.deleteRow(iy)
				grid.insertRow(destY)
				assert(dims != Nil)
				grid.yDimensionLists(destY) = dims
				pasteRow(destY, r)
				_block match {
					case labels: DimensionLabelsBlock => {
						val col = grid.column(ix)
						grid.xDimensionLists(ix) = grid.reorderDisplayDimensions(col)
					}
					case _ =>;//ignore
				}
			}
			case DimensionRotatedHorizontal(yGap) => {
				val dim = _block.asInstanceOf[DimensionLabelsBlock].displayDimension
				val widthDimension = grid.xDimensionLists(_block.xIndex)
				// Remove existing blocks
				for (yRow <- _pivotRows) {
					for(b <- grid.row(yRow)) b match {
						case labs: DimensionLabelsBlock if (labs.displayDimension==dim) => grid(b.xIndex, yRow) = None
						case _ => ;//ignore
					}
					val newRowDimensions = grid.reorderDisplayDimensions(grid.row(yRow))
					grid.yDimensionLists(yRow) = if (newRowDimensions.isEmpty) widthDimension else newRowDimensions
				}
				// Insert new row
				grid.insertRow(yGap)
				grid.yDimensionLists(yGap) = widthDimension
				// Populate with new label blocks
				for (xCol <- _pivotColumns) {
					grid(xCol, yGap) = Some(new DimensionLabelsBlock(dim))
					grid.xDimensionLists(xCol) = grid.reorderDisplayDimensions(grid.column(xCol))
				}
				// Tighten up any gaps
				grid.compress
				// Bob's your uncle
				control.computeBounds
			}
			case DimensionRotatedVertical(xGap) => {
				val dim = _block.asInstanceOf[DimensionLabelsBlock].displayDimension
				val widthDimension = grid.yDimensionLists(_block.yIndex)
				for(xCol <- _pivotColumns){
					for(b <- grid.column(xCol)) b match {
						case labs: DimensionLabelsBlock if (labs.displayDimension==dim) => grid(xCol, b.yIndex) = None
						case _=> ;// ignore
					}
					val newColDimensions = grid.reorderDisplayDimensions(grid.column(xCol))
					grid.xDimensionLists(xCol) = if(newColDimensions.isEmpty) widthDimension else newColDimensions
				}
				// Tighten up any gaps
				// Insert new column
				grid.insertCol(xGap)
				grid.xDimensionLists(xGap) = widthDimension
				// And add in new label blocks
				for (yRow <- _pivotRows) {
					grid(xGap, yRow) = Some(new DimensionLabelsBlock(dim))
					grid.yDimensionLists(yRow) = grid.reorderDisplayDimensions(grid.row(yRow))
				}
				// Tighten up any gaps
				grid.compress
				// Bob's your uncle
				control.computeBounds
			}
		}
	}

	private def pasteRow(iy: Int, row: RandomAccessSeq[DisplayBlock]) {
		for(x <- 0 until grid.xGridSize)
			grid(x, iy) = Some(row(x))
	}

	private def pasteColumn(ix: Int, col: RandomAccessSeq[DisplayBlock]) {
		for(y <- 0 until grid.yGridSize)
			grid(ix, y) = Some(col(y))
	}

}
