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
import ui.selection.{Selectable, NullSelection}

class StructBlock extends Block {
	var orientation: Orientation = XOrientation
	var structAxis: StructAxis = null
	var elements: Seq[Block] = Nil
	var labelDepths: Array[Int] = null
	var endsX: Array[Int] = null
	var endsY: Array[Int] = null
	
	var maxElementDepth: Int = 0

	def computeInnerSizeAndHeaders {
		for(el <- elements) el.computeSize

		maxElementDepth = (0 /: elements)((depth, el) => 
				Math.max(depth, el.innerDepth(orientation))
			)
		endsX = new Array[Int]((1 /: xAxes)(_ * _.length))
		endsY = new Array[Int]((1 /: yAxes)(_ * _.length))

		topHeader = headerDepth(XOrientation, xAxes, None)
		leftHeader = headerDepth(YOrientation, yAxes, None)
		innerSize = new Point(
			sizeOf(XOrientation, xAxes, endsX),
			sizeOf(YOrientation, yAxes, endsY))
	}

	override def labelDepth(o: Orientation, a: Axis, i: Int) =
		if (a == structAxis && isPrimaryAxis(o))
			labelDepths(i)
		else
			if (o.isX) xLabelHeight(a, i) else yLabelWidth(a, i)


	override def isPrimaryAxis(o: Orientation) = (o == orientation)

	def ends(o: Orientation) = if (o.isX) endsX else endsY

	def headerDepth(o: Orientation, axes: Seq[Axis], element: Option[Block]): Int = {
		if (!axes.isEmpty){
			val remainingAxes = axes.drop(1)
			axes(0) match {
				case a: ArrayAxis =>
					val header = headerDepth(o, remainingAxes, element)
					val maxLabelDepth = (0 /: a.range)((max, i)=>
						Math.max(max, preferredLabelDepth(orientation, a, i)))
					header + maxLabelDepth

				case s: StructAxis =>
					assert(isPrimaryAxis(o), "StructAxes only allowed on primary dimension (by definition).")
					assert(s == structAxis, "Must be the correct instance of StructAxis")
					assert(element.isEmpty, "Must not have already found a StructAxis")
					val labelChildDepths = new Array[Int](s.length)
					val maxDepth = (0 /: s.range)((maxHead, i) => {
						val structLabelDepth = preferredLabelDepth(o, s, i)
						val elHead = headerDepth(o, remainingAxes, Some(elements(i)))
						labelChildDepths(i) = elHead
						Math.max(maxHead, elHead + structLabelDepth)
					})
					labelDepths = labelChildDepths // Ugly hack to avoid allocation. (Modifies array in-place.)
					for (i <- s.range)
						labelDepths(i) = maxDepth - labelChildDepths(i)

					maxDepth

				case _=> error("Don't recognise that kind of Axis")
			}
			
		}else {
			if (isPrimaryAxis(o))
				element.getOrElse(error("StructBlock primary axis must contain a StructAxis!"))
					.nearHeader(o)

			else
				0
		}
	}

	def sizeOf(o: Orientation, axes: Seq[Axis], ends: Array[Int]): Int = {
		var p = 0
		iterateValues(0, axes, Map.empty, (b0, cellCoords)=>{
			val b = breadthOfCell(o, cellCoords)
			ends(p) = b0 + b; p += 1
			b
		})
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

	override def hitTestChildLabels(parent: Map[Axis,Int], o: Orientation, b: Int, d: Int) = {
		if (isPrimaryAxis(o)){
			val el = elements(parent(structAxis))
			el.hitTestLabels(parent, o, b + el.firstHeader(o), d)

		}else
			super.hitTestChildLabels(parent, o, b, d)
	}


	def hitTestAxis(o: Orientation, b: Int): Option[(Map[Axis,Int], Int)] = {
		var totalBreadth = innerBreadth(o)
		if (b >= 0 && b < totalBreadth) {
			val e = ends(o)
			val n0 = e.length
			val i0 = binarySearch(e, b)
			val remainder = if (i0 == 0) b else b - e(i0-1)
			var coords: Map[Axis,Int] = Map.empty
			var n = n0
			var i = i0
			for(ax <- axes(o)) {
				val width = n / ax.length
				coords += ((ax, i / width))
				i %= width
				n = width
			}
			Some((coords, remainder))
		}else
			None
	}


	def hitTestCell(parent: Map[Axis,Int], rel: Point) = {
		val el = elements(parent(structAxis))
		val innerHead = el.firstHeader(orientation)
		val (b, d) = orientation.breadthDepth(rel)
		if (b < innerHead) {// header area
			el.hitTestLabels(parent, orientation.opposite, d, b - innerHead)

		} else {
			val dataRel = orientation.newPoint(b - innerHead, d)
			el.hitTestCells(parent, dataRel)
				.orElse(CellSelection(parent))
		}
	}

	
	def binarySearch(ends: Array[Int], v: Int): Int = {
		def search(lo: Int, hi: Int): Int = {
			if (lo == hi)
				lo
			else{
				val mid = lo + (hi - lo)/2
				if (v < ends(mid))
					search(lo, mid)
				else
					search(mid+1, hi)
			}
		}
		search(0, ends.length)
	}

}
