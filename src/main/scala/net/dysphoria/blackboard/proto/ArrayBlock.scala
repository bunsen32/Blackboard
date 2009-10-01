/*
 * ArrayBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import org.eclipse.swt.graphics._
import ui.{Orientation, XOrientation, YOrientation}
import gfx._

class ArrayBlock extends Block {
	var array: ArrayTable = null
	val cellStyle = new CellStyle

	def computeInnerSizeAndHeaders {
		val width = sizeOf(XOrientation, xAxes)
		val height = sizeOf(YOrientation, yAxes)
		innerSize = new Point(width, height)
		topHeader = (0 /: xAxes)(_ + xLabelHeight(_, 0))
		leftHeader = (0 /: yAxes)(_ + yLabelWidth(_, 0))
	}

	def sizeOf(orientation: Orientation, axes: Seq[Axis]): Int = {
		if (!axes.isEmpty) {
			val remainingAxes = axes.drop(1)
			axes(0) match {
				case a: ArrayAxis =>
					val breadth = sizeOf(orientation, remainingAxes)
					breadth * a.length // unit breadth × size of dimension

				case _ => error("ArrayBlock contains non-Array Axis!")
			}
			
		}else{
			breadthOfCell(orientation, null)
		}
	}

	def hitTest(orientation: Orientation, axes: Seq[Axis], p: Int): Option[(Coord, Int)] = {
		None
	}

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]): Int =
		orientation.choose(genericCellWidth, genericCellHeight)


	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int]){
		val selected = gfx.ui.selection match {
			case CellSelection(coords) => coords == indices
			case _=> false
		}
		renderBasicCell(gfx, cellStyle, bounds,
						array(indices).toString, selected)
	}

	def hitTestAxis(o: Orientation, b: Int): Option[(Map[Axis,Int], Int)] = {
		var remainder = b
		var available = innerBreadth(o)
		var coords: Map[Axis, Int] = Map.empty
		for(ax <- axes(o)) {
			val perValue = available / ax.length
			val v = remainder / perValue
			coords += ((ax, v))
			remainder %= perValue
			available = perValue
		}
		Some((coords, remainder))
	}

	def hitTestCell(coords: Map[Axis,Int], relative: Point) =
		CellSelection(coords)

}
