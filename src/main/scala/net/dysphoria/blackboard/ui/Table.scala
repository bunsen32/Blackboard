/*
 * Table.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.{Point, Rectangle}
import gfx._
import selection._

class Table(var topBlock: TableBlock) extends Displayable {
	def computeSize {
		topBlock.computeSize
	}

	def size = topBlock.outerSize


	def render(g: DrawingContext, origin: Point) = {
		val dataOrigin = new Point(origin.x + topBlock.leftHeader,
								   origin.y + topBlock.topHeader)

		topBlock.renderCells(g, dataOrigin, Map.empty)
		//Headers
		topBlock.renderLabels(g, dataOrigin, Horizontal, Map.empty)
		topBlock.renderLabels(g, dataOrigin, Vertical, Map.empty)
	}


	def hitTest(parent: Map[Axis, Int], p: Point): Selectable = {
		if (p.x >= 0 && p.x < topBlock.outerSize.x && p.y >= 0 && p.y < topBlock.outerSize.y) {
			val x = (p.x - topBlock.leftHeader)
			val y = (p.y - topBlock.topHeader)
			if (x < 0 && y < 0)
				NullSelection

			else if (x < 0)
				topBlock.hitTestLabels(parent, Vertical, y, x)

			else if (y < 0)
				topBlock.hitTestLabels(parent, Horizontal, x, y)

			else
				topBlock.hitTestCells(parent, new Point(x, y))

		}else
			NullSelection
	}


	def moveByCell(sel: SingleGridSelection, o: Orientation, d: Direction) = {
		topBlock.moveByOne(sel, o, d) orElse {
			sel match {
				case label: OneLabel if topBlock containsInEdgeArea label =>
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
		val x = topBlock.breadthCellBounds(topBlock.leftHeader, Horizontal, coords)
		val y = topBlock.breadthCellBounds(topBlock.topHeader, Vertical, coords)
		return new Rectangle(x.start, y.start, x.length, y.length)
	}

	def labelBounds(lab: OneLabel): Rectangle = {
		topBlock.labelBounds(new Point(topBlock.leftHeader, topBlock.topHeader), lab)
	}


	def arrayTable(coords: Map[Axis,Int]) = topBlock.arrayTable(coords)

}
