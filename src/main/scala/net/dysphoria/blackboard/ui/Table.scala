/*
 * Table.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.{Point, Rectangle}
import gfx._
import selection._

class Table(val topBlock: TableBlock) extends Displayable {
	import topBlock._

	def computeSize {
		topBlock.computeSize
	}

	def size = topBlock.outerSize


	def render(g: DrawingContext, origin: Point) = {
		val dataOrigin = new Point(origin.x + leftHeader, origin.y + topHeader)

		renderCells(g, dataOrigin, Map.empty)
		//Headers
		renderLabels(g, dataOrigin, XOrientation, Map.empty)
		renderLabels(g, dataOrigin, YOrientation, Map.empty)
	}


	def hitTest(parent: Map[Axis, Int], p: Point): Selectable = {
		if (p.x >= 0 && p.x < outerSize.x && p.y >= 0 && p.y < outerSize.y) {
			val x = (p.x - leftHeader)
			val y = (p.y - topHeader)
			if (x < 0 && y < 0)
				NullSelection

			else if (x < 0)
				hitTestLabels(parent, YOrientation, y, x)

			else if (y < 0)
				hitTestLabels(parent, XOrientation, x, y)

			else
				hitTestCells(parent, new Point(x, y))

		}else
			NullSelection
	}


	def moveByCell(sel: SingleGridSelection, o: Orientation, d: Direction) = {
		topBlock.moveByOne(sel, o, d) orElse {
			sel match {
				case label: LabelSelection if topBlock containsInEdgeArea label =>
					if (o == label.orientation.opposite && d.isForward)
						topBlock.selectEdgeChild(Map.empty, o, First, sel)
					else
						NullSelection

				case _ =>
					if (d.isBack)
						topBlock.selectEdgeLabel(Map.empty, o.opposite, o, Last, sel)
					else
						 NullSelection
			}
		}
	}


	def cellBounds(coords: Map[Axis, Int]): Rectangle = {
		val x = breadthCellBounds(leftHeader, XOrientation, coords)
		val y = breadthCellBounds(topHeader, YOrientation, coords)
		return new Rectangle(x.start, y.start, x.length, y.length)
	}

	def labelBounds(lab: LabelSelection): Rectangle = {
		topBlock.labelBounds(new Point(leftHeader, topHeader), lab)
	}


	def arrayTable(coords: Map[Axis,Int]) = topBlock.arrayTable(coords)

}
