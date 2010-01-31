/*
 * Table.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.model

import org.eclipse.swt.graphics.{Point, Rectangle}
import net.dysphoria.blackboard._
import gfx.{CellStyle, DrawingContext}
import ui._
import ui.Origin._
import ui.selection._
import org.eclipse.swt.SWT

class Table(var topBlock: TablePart) extends Displayable {
	val titleHeight = 19*256 // Need to get rid of these at some point.
	val titleStyle = new CellStyle {
		fontStyle = SWT.ITALIC
		backgroundColor = ViewCanvas.backgroundColour
	}

	def computeSize {
		topBlock.computeSize
		val contentWidth = topBlock.innerSize.x + topBlock.leftHeader + topBlock.rightHeader
		val contentHeight = topBlock.innerSize.y + topBlock.topHeader + topBlock.bottomHeader
		size = new Point(contentWidth,	 contentHeight + titleHeight)
	}

	var _size: Point = Origin
	def size = _size
	private def size_=(newSz: Point) = _size = newSz

	var title: String = "table"

	def render(g: DrawingContext, origin: Point, context: Map[Axis, Int]) = {
		val dataOrigin = new Point(origin.x + topBlock.leftHeader,
								   origin.y + topBlock.topHeader + titleHeight)
		val isSelected = g.ui.selection match {
			case selectedTable: TableInstance => instanceIs(context, selectedTable)
			case _ => false
		}

		val titleBounds = new Rectangle(origin.x, origin.y, size.x, titleHeight)
		g.renderBasicCell(titleStyle, titleBounds, title, (if(isSelected)2 else 0), false)

		g.gc.setBackground(g.gc.getDevice.getSystemColor(SWT.COLOR_GRAY))
		g.gc.fillRectangle(origin.x, origin.y + titleHeight, size.x, size.y - titleHeight)
		topBlock.renderCells(g, dataOrigin, context)
		//Headers
		topBlock.renderLabels(g, dataOrigin, Horizontal, context)
		topBlock.renderLabels(g, dataOrigin, Vertical, context)
	}

	private def labelBounds(origin: Point, lab: LabelInstance): Rectangle = {
		topBlock.labelBounds(new Point(origin.x + topBlock.leftHeader, origin.y + titleHeight + topBlock.topHeader), lab)
	}

	private def cellBounds(origin: Point, cell: AbstractCellInstance): Rectangle = {
		val x = topBlock.breadthCellBounds(topBlock.leftHeader, Horizontal, cell.coords)
		val y = topBlock.breadthCellBounds(topBlock.topHeader, Vertical, cell.coords)
		new Rectangle(origin.x + x.start, origin.y + titleHeight + y.start, x.length, y.length)
	}

	override type Instance = TableInstance

	def instance(container: DisplayableContainer, coords: Map[Axis, Int]) = TableInstance(container, coords)

	case class TableInstance(val container: DisplayableContainer, coords: Map[Axis,Int]) extends DisplayableInstance with TableBuildingBlock with TableItemSelection {
		override def model = Table.this
		def table = this
		type ComponentType = TablePart#TablePartInstance
		lazy val topPart = model.topBlock.instance(this, coords)

		override def hitTest(p: Point): Selectable = {
			if (p.x >= 0 && p.x < size.x && p.y >= 0 && p.y < size.y) {
				if (p.y < titleHeight)
					this
					
				else {
					val x = (p.x - topPart.leftHeader)
					val y = (p.y - topPart.topHeader - titleHeight)
					if (x < 0 && y < 0)
						this

					else if (x < 0)
						topPart.hitTestLabels(Vertical, y, x)

					else if (y < 0)
						topPart.hitTestLabels(Horizontal, x, y)

					else
						topPart.hitTestCells(new Point(x, y))
				}

			}else
				NullSelection
		}

		def oneBeyond(sel: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints) =
			 None

		def oneBeyondContent(cell: TablePart#TablePartInstance, direction: CompassPosition)(implicit hint: SelectionHints) = {
			assert(cell == topPart)
			if (direction.forwardBack.isBack)
				topPart.edgeLabel(direction.orientation.other, direction.opposite)
			else
				None
		}

		def oneBeyondLabelArea(childPartLabel: UnnestedLabelInstance, direction: CompassPosition)(implicit hint: SelectionHints) = {
			val childPart = childPartLabel.containingPart
			val axisOrientation = childPartLabel.orientation
			assert(childPart == topPart)
			if (axisOrientation == Horizontal && direction == Down)
				topPart.edgeCell(Up)
			else if (axisOrientation == Vertical && direction == Right)
				topPart.edgeCell(Left)
			else
				None
		}

		def boundsOf(item: SingleGridSelection): Rectangle = item match {
			case label: LabelInstance => model.labelBounds(position, label)
			case cell: AbstractCellInstance => model.cellBounds(position, cell)
		}
	}

	def instanceIs(instanceCoords: Map[Axis,Int], table: Table#TableInstance): Boolean =
		(table.model == this && instanceCoords == table.coords)

}

object Table {
	implicit def instanceToTable(instance: Table#Instance) = instance.model
}