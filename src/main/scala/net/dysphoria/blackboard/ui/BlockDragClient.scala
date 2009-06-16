/*
 * BlockDragClient.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

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
	var _currentTargetControl: Option[GridView] = None
	var _dropTarget: DropTarget = NoDropTarget

	override def start(op: DragOperation, m: MouseDown) {
		// TODO: we're dragging the 'selection', but the MouseDown(x,y) is relative
		// to the current focus object (Which should nonetheless be part of the
		// selection.)
		ui.dragState = Dragging
		op.size = selection.block.size
		op.dragAnchor = new Point(m.x, m.y)
	}
	override def paint(gc: GC) {
		selection.block.render(new DrawingContext(gc, ui), new Point(0, 0))
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

			def rotationDropTarget: DropTarget = selection.block match {
				case block: DimensionLabelsBlock => {
					if (block.isXNotY)
						if (iy >=0 && iy < grid.yDimensionLists.length &&
							(ix != srcX || !preferHoriz) && iy != srcY)
							DimensionRotatedVertical(xGap, iy)
						else
							NoDropTarget
							
					else
						if (ix >= 0 && ix < grid.xDimensionLists.length &&
							(iy != srcY || preferHoriz) && ix != srcX)
							DimensionRotatedHorizontal(ix, yGap)
						else
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
	case class DimensionRotatedHorizontal(xCol: Int, yGap: Int) extends DropTarget
	case class DimensionRotatedVertical(xGap: Int, yRow: Int) extends DropTarget

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
				case DimensionRotatedHorizontal(xCol, yGap) => {
					val x = grid.xDimensionLists.gapPosition(xCol)
					val y = grid.yDimensionLists.gapPosition(yGap)
					val w = DisplayDimension.widthOf(grid.xDimensionLists(xCol))
					drawLine(x, y, x + w, y)
				}
				case DimensionRotatedVertical(xGap, yRow) => {
					val x = grid.xDimensionLists.gapPosition(xGap)
					val y = grid.yDimensionLists.gapPosition(yRow)
					val h = DisplayDimension.widthOf(grid.yDimensionLists(yRow))
					drawLine(x, y, x, y + h)
				}
			}
		}
		override def size = control.getSize
	}

	override def commit {
		val block = selection.block
		val ix = selection.ix
		val iy = selection.iy
		dropTarget match {
			case NoDropTarget => ;// Nothing to draw
			case ColumnMovedTo(xGap) => {
				val destX = if (ix < xGap) xGap - 1 else xGap
				val dims = grid.xDimensionLists(ix)
				val col = copyColumn(ix)
				val oldWidth = grid.xGridSize
				grid.deleteCol(ix)
				grid.insertCol(destX)
				assert(dims != Nil)
				grid.xDimensionLists(destX) = dims
				pasteColumn(destX, col)
				block match {
					case labels: DimensionLabelsBlock => {
						val row = copyRow(iy)
						grid.yDimensionLists(iy) = grid.reorderDisplayDimensions(row)
					}
					case _=>;//ignore
				}
			}
			case RowMovedTo(yGap) => {
				val destY = if (iy < yGap) yGap - 1 else yGap
				val dims = grid.yDimensionLists(iy)
				val row = copyRow(iy)
				grid.deleteRow(iy)
				grid.insertRow(destY)
				assert(dims != Nil)
				grid.yDimensionLists(destY) = dims
				pasteRow(destY, row)
				block match {
					case labels: DimensionLabelsBlock => {
						val col = copyColumn(ix)
						grid.xDimensionLists(ix) = grid.reorderDisplayDimensions(col)
					}
					case _ =>;//ignore
				}
			}
			case DimensionRotatedHorizontal(x, y) => ; //ooh, complex
			case DimensionRotatedVertical(x, y) => ; //ooh, compled too
		}
	}

	private def copyColumn(ix: Int): Array[DisplayBlock] = {
		val col = new Array[DisplayBlock](grid.yGridSize)
		for(y <- 0 until grid.yGridSize)
			col(y) = grid(ix, y)
		col
	}
	private def pasteColumn(ix: Int, col: Array[DisplayBlock]) {
		for(y <- 0 until grid.yGridSize)
			grid(ix, y) = Some(col(y))
	}

	private def copyRow(iy: Int): Array[DisplayBlock] = {
		val row = new Array[DisplayBlock](grid.xGridSize)
		for(x <- 0 until grid.xGridSize)
			row(x) = grid(x, iy)
		row
	}
	private def pasteRow(iy: Int, row: Array[DisplayBlock]) {
		for(x <- 0 until grid.xGridSize)
			grid(x, iy) = Some(row(x))
	}
	
}
