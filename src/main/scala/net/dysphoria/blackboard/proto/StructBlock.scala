/*
 * StructBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import org.eclipse.swt.graphics._
import ui.{Orientation, XOrientation, YOrientation}
import gfx._

class StructBlock extends Block {
	var orientation: Orientation = XOrientation
	var structAxis: StructAxis = null
	var elements: Seq[Block] = Nil
	var labelDepths: scala.Array[Int] = null
	
	var maxElementDepth: Int = 0

	def computeInnerSizeAndHeaders {
		for(el <- elements) el.computeSize

		maxElementDepth = (0 /: elements)((depth, el) => {
				val elInner = el.innerSize
				Math.max(depth, el.innerDepth(orientation))
			})
		
		val (topHead, width) = sizeOf(XOrientation, xAxes, None)
		val (sideHead, height) = sizeOf(YOrientation, yAxes, None)
		topHeader = topHead
		leftHeader = sideHead
		innerSize = new Point(width, height)
	}

	override def labelDepth(o: Orientation, a: Axis, i: Int) =
		if (a == structAxis && isPrimaryAxis(o))
			labelDepths(i)
		else
			if (o.isX) xLabelHeight(a, i) else yLabelWidth(a, i)


	override def isPrimaryAxis(o: Orientation) = (o == orientation)

	def sizeOf(o: Orientation, axes: Seq[Axis], element: Option[Block]): (Int, Int) = {
		if (!axes.isEmpty){
			val remainingAxes = axes.drop(1)
			axes(0) match {
				case a: ArrayAxis =>
					val (header, breadth) = sizeOf(o, remainingAxes, element)
					(header + preferredLabelDepth(orientation, a, 0),
					 breadth * a.length)

				case s: StructAxis =>
					assert(isPrimaryAxis(o), "StructAxes only allowed on primary dimension (by definition).")
					assert(s == structAxis, "Must be the correct instance of StructAxis")
					assert(element.isEmpty, "Must not have already found a StructAxis")
					val labelChildDepths = new scala.Array[Int](s.length)
					val result = ((0, 0) /: s.range)((aggregate, i) => {
						val structLabelDepth = preferredLabelDepth(o, s, i)
						val el = elements(i)
						val (agHead, agBreadth) = aggregate
						val (elHead, elBreadth) = sizeOf(o, remainingAxes, Some(el))
						labelChildDepths(i) = elHead
						(Math.max(agHead, elHead + structLabelDepth),
						 agBreadth + elBreadth)
					})
					val (maxDepth, _) = result
					labelDepths = labelChildDepths // Ugly hack to avoid allocation. Updates array in-place
					for (i <- s.range)
						labelDepths(i) = maxDepth - labelChildDepths(i)

					result

				case _=> error("Don't recognise that kind of Axis")
			}
			
		}else {
			if (isPrimaryAxis(o)){
				val el = element.getOrElse(error("StructBlock primary axis must contain a StructAxis!"))
				(el.nearHeader(o),
				 el.outerBreadth(o))

			}else {
				(0, maxElementDepth)
			}
		}
	}

	def breadthOfCell(o: Orientation, c: Map[Axis, Int]): Int = {
		if (isPrimaryAxis(o))
			elements(c(structAxis)).outerBreadth(o)
		else
			maxElementDepth
	}

	override def renderChildLabels(gfx: DrawingContext, o: Orientation, b0: Int, d0: Int, availableDepth: Int,
						  coords: Map[Axis, Int]): Int = {

		if (isPrimaryAxis(o)){
			val el = elements(coords(structAxis))
			val origin = o.newPoint(b0 + el.firstHeader(o), d0)
			el.renderLabels(gfx, origin, o, coords)
		}
		breadthOfCell(o, coords)
	}

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int]){
		val el = elements(indices(structAxis))
		val headerDistance = el.firstHeader(orientation)
		val dataOrigin = if (orientation.isX)
				new Point(bounds.x + el.leftHeader, bounds.y)
			else
				new Point(bounds.x, bounds.y + el.topHeader)

		el.renderCells(gfx, dataOrigin, indices)
		el.renderLabels(gfx, dataOrigin, orientation.opposite, indices)
	}

}
