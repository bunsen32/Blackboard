/*
 * ArrayBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import net.dysphoria.blackboard._
import gfx._
import selection._

class ArrayBlock(val table: Table) extends TableBlock {
	var array = new FlexibleArrayTable(Nil)
	val cellStyle = new CellStyle
	val voidCellStyle = new CellStyle {
		backgroundColor = new RGB(200, 200, 230)
	}

	def showEmptyAxes = true

	def computeInnerSizeAndHeaders {
		val width = sizeOf(Horizontal, xAxes)
		val height = sizeOf(Vertical, yAxes)
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
					breadth * visibleLength(a) // unit breadth × size of dimension

				case _ => error("ArrayBlock contains non-Array Axis!")
			}
			
		}else{
			breadthOfCell(orientation, null)
		}
	}

	def labelDepth(labelOrientation: Orientation, a: Axis, i: Int) =
		if (labelOrientation.isX) xLabelHeight(a, i) else yLabelWidth(a, i)

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int]){
		val withinData = !indices.exists(pair => pair._2 >= pair._1.length)
		val selected = gfx.ui.selection match {
			case CellSelection(_, coords) => if (coordinatesMatch(indices, coords)) 2 else 0
			case _=> 0
		}
		if (withinData){
			renderBasicCell(gfx, cellStyle, bounds,
							array(indices).toString, selected, false)
		}else{
			renderBasicCell(gfx, voidCellStyle, bounds, "", selected, false)
		}
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
			val perValue = available / visibleLength(ax)
			val v = remainder / perValue
			coords += ((ax, v))
			remainder %= perValue
			available = perValue
		}
		Some((coords, remainder))
	}

	def hitTestCell(coords: Map[Axis,Int], relative: Point) =
		CellSelection(table, coords)


	def arrayTable(coords: Map[Axis,Int]) = array

	
	/*------------------------------------------------------------------------*/
	// NAVIGATION

	def containsInEdgeArea(sel: OneLabel) = (sel.block == this)

	
	def moveByOne(sel: SingleGridSelection, o: Orientation, d: Direction): Selectable = sel match {
		case lab: OneLabel if this containsInEdgeArea lab =>
			moveOwnLabelByOne(lab, o, d)

		case _ =>
			nextOnAxes(axes(o), sel.coords, d) match {
				case Some(coords) => CellSelection(sel.table, coords)
				case None => NullSelection
			}
	}


	def selectEdgeChild(context: Map[Axis,Int], plane: Orientation, end: End, hintSel: SingleGridSelection): Selectable =
		CellSelection(table, context
					  ++ axesEnd(plane, end)
					  ++ hintCoords(axes(plane.opposite), hintSel.coords))



	/*------------------------------------------------------------------------*/
	// SIZING

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]): Int =
		orientation.choose(genericCellWidth, genericCellHeight)


	def breadthCellBounds(offset: Int, o: Orientation, coords: Map[Axis,Int]): Range = {
		val breadth = breadthOfCell(o, coords) // Assume all cells same width
		val b0 = cellIndexOf(o, coords) * breadth + offset
		val b = 1 * breadth // Cell count must == 1
		new Range(b0, (b0 + b), 1)
	}


	def breadthOwnLabelBounds(offset: Int, o: Orientation, coords: Map[Axis,Int], num: Int) = {
		val i = cellIndexOf(o, coords)
		val breadth = breadthOfCell(o, coords) // Assume all cells same width
		val b0 = offset + (i * breadth)
		new Range(
			b0,
			b0 + (num * breadth), 1)
	}


	def childLabelBounds(dataOrigin: Point, lab: OneLabel) =
		error("childLabelBounds: ArrayBlock does not have child elements.")


	/*------------------------------------------------------------------------*/

	override def toString = "ArrayBlock"
}
