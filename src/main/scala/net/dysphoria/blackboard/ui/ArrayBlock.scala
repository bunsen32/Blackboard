/*
 * ArrayBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import gfx._
import selection._

class ArrayBlock extends TableBlock {
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

	def labelDepth(labelOrientation: Orientation, a: Axis, i: Int) =
		if (labelOrientation.isX) xLabelHeight(a, i) else yLabelWidth(a, i)

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int]){
		val selected = gfx.ui.selection match {
			case CellSelection(coords) => coordinatesMatch(indices, coords)
			case _=> false
		}
		renderBasicCell(gfx, cellStyle, bounds,
						array(indices).toString, selected)
	}

	
	/*------------------------------------------------------------------------*/
	// HIT TESTING

	// Don't have any child labels, so just return selection as-is
	def hitTestChildLabels(parent: Map[Axis,Int], o: Orientation, b: Int, d: Int) =
		NullSelection

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


	def arrayTable(coords: Map[Axis,Int]) = array

	
	/*------------------------------------------------------------------------*/
	// NAVIGATION

	def containsInEdgeArea(sel: LabelSelection) = (sel.block == this)

	
	def moveByOne(sel: SingleGridSelection, o: Orientation, d: Direction): Selectable = sel match {
		case lab: LabelSelection if this containsInEdgeArea lab =>
			moveOwnLabelByOne(lab, o, d)

		case _ =>
			nextOnAxes(axes(o), sel.coords, d) match {
				case Some(coords) => CellSelection(coords)
				case None => NullSelection
			}
	}


	def selectEdgeChild(context: Map[Axis,Int], plane: Orientation, end: End, hintSel: SingleGridSelection): Selectable = 
		CellSelection(context 
					  ++ axesEnd(plane, end)
					  ++ hintCoords(axes(plane.opposite), hintSel.coords))



	/*------------------------------------------------------------------------*/
	// SIZING

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]): Int =
		orientation.choose(genericCellWidth, genericCellHeight)


	def breadthCellBounds(offset: Int, o: Orientation, coords: Map[Axis,Int]): Range = {
		val i = cellIndexOf(o, coords)
		val breadth = breadthOfCell(o, coords)
		val b0 = offset + i * breadth // Assume all cells same width
		new Range(b0, b0 + breadth, 1)
	}


}
