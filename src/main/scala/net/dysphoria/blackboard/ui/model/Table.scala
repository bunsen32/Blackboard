/*
 * Table.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.model

import org.eclipse.swt.graphics.{Point, Rectangle}
import net.dysphoria.blackboard._
import gfx.DrawingContext
import ui._
import ui.selection._

class Table(var topBlock: TablePart) extends Displayable {
	def computeSize {
		topBlock.computeSize
	}

	def size = topBlock.outerSize


	def render(g: DrawingContext, origin: Point, context: Map[Axis, Int]) = {
		val dataOrigin = new Point(origin.x + topBlock.leftHeader,
								   origin.y + topBlock.topHeader)

		topBlock.renderCells(g, dataOrigin, context)
		//Headers
		topBlock.renderLabels(g, dataOrigin, Horizontal, context)
		topBlock.renderLabels(g, dataOrigin, Vertical, context)
	}

	def moveByCell(sel: SingleGridSelection, o: Orientation, d: Direction) = {
		NullSelection
/* TODO: navigation
		topBlock.moveByOne(sel, o, d) orElse {
			sel match {
				case label: LabelInstance if topBlock containsInEdgeArea label =>
					if (o == label.orientation.other && d.isForward)
						topBlock.selectEdgeChild(Map.empty, o, First, sel)
					else
						NullSelection

				case _ =>
					if (d.isBack)
						topBlock.selectEdgeLabel(Map.empty, o.other, o, Last, sel)
					else
						 NullSelection
			}
		}
*/
	}

	def boundsOf(origin: Point, sel: DataSelection) = sel match {
		case label: LabelInstance => labelBounds(origin, label)
		
		case labels: LabelRange =>
			val r1 = labelBounds(origin, labels.first)
			val r2 = labelBounds(origin, labels.last)
			r1.union(r2)

		case cell: AbstractCellInstance => 
			val x = topBlock.breadthCellBounds(topBlock.leftHeader, Horizontal, cell.coords)
			val y = topBlock.breadthCellBounds(topBlock.topHeader, Vertical, cell.coords)
			new Rectangle(origin.x + x.start, origin.y + y.start, x.length, y.length)
	}

	private def labelBounds(origin: Point, lab: LabelInstance): Rectangle = {
		topBlock.labelBounds(new Point(origin.x + topBlock.leftHeader, origin.y + topBlock.topHeader), lab)
	}

	override type Instance = TableInstance

	def instance(coords: Map[Axis, Int]) = TableInstance(coords)

	case class TableInstance(coords: Map[Axis,Int]) extends DisplayableInstance with TableBuildingBlock with TableItemSelection {
		override def model = Table.this
		def table = this
		def container = throw new NotImplementedException
		type ComponentType = TablePart#TablePartInstance
		lazy val topPart = model.topBlock.instance(this, coords)

		override def hitTest(p: Point): Selectable = {
			if (p.x >= 0 && p.x < topPart.outerSize.x && p.y >= 0 && p.y < topPart.outerSize.y) {
				val x = (p.x - topPart.leftHeader)
				val y = (p.y - topPart.topHeader)
				if (x < 0 && y < 0)
					NullSelection

				else if (x < 0)
					topPart.hitTestLabels(Vertical, y, x)

				else if (y < 0)
					topPart.hitTestLabels(Horizontal, x, y)

				else
					topPart.hitTestCells(new Point(x, y))

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
	}

}

object Table {
	implicit def instanceToTable(instance: Table#Instance) = instance.model
}