/*
 * StructBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import org.eclipse.swt.graphics._
import gfx._
import ui.selection._

class StructBlock(val structAxis: StructAxis) extends TableBlock {
	var orientation: Orientation = Horizontal
	var elements = new mutable.ArrayBuffer[TableBlock]
	var labelDepths: Array[Int] = null
	var endsX: Array[Int] = null
	var endsY: Array[Int] = null
	
	var maxElementDepth: Int = 0

	/*------------------------------------------------------------------------*/
	// SIZING

	def computeInnerSizeAndHeaders {
		for(el <- elements) el.computeSize

		maxElementDepth = (0 /: elements)((depth, el) => 
				Math.max(depth, el.innerDepth(orientation))
			)
		endsX = new Array[Int]((1 /: xAxes)(_ * _.length))
		endsY = new Array[Int]((1 /: yAxes)(_ * _.length))

		topHeader = headerDepth(Horizontal, xAxes, None)
		leftHeader = headerDepth(Vertical, yAxes, None)
		innerSize = new Point(
			sizeOf(Horizontal, xAxes, endsX),
			sizeOf(Vertical, yAxes, endsY))
	}

	def labelDepth(o: Orientation, a: Axis, i: Int) =
		if (a == structAxis && isPrimaryAxis(o))
			labelDepths(i)
		else
			if (o.isX) xLabelHeight(a, i) else yLabelWidth(a, i)


	override def isPrimaryAxis(o: Orientation) = (o == orientation)

	def starts(o: Orientation)(i: Int) = if (i == 0) 0 else ends(o)(i - 1)
	def ends(o: Orientation) = if (o.isX) endsX else endsY

	def headerDepth(o: Orientation, axes: Seq[Axis], element: Option[TableBlock]): Int = {
		if (!axes.isEmpty){
			val remainingAxes = axes.drop(1)
			axes.first match {
				case a: ArrayAxis =>
					val header = headerDepth(o, remainingAxes, element)
					val maxLabelDepth = (0 /: a.range)((max, i)=>
						Math.max(max, preferredLabelDepth(o, a, i)))
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

	
	/*------------------------------------------------------------------------*/
	// RENDERING

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


	/*------------------------------------------------------------------------*/
	// HIT-TESTING

	def hitTestChildLabels(parent: Map[Axis,Int], o: Orientation, b: Int, d: Int) = {
		if (isPrimaryAxis(o)){
			val el = elements(parent(structAxis))
			el.hitTestLabels(parent, o, b - el.firstHeader(o), d)

		}else
			NullSelection
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
				.orElse(BlockSelection(parent))
		}
	}

	
	private def binarySearch(ends: Array[Int], v: Int): Int = {
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

	
	/*------------------------------------------------------------------------*/
	// NAVIGATION

	def containsInEdgeArea(sel: OneLabel) =
		(sel.block == this) ||
		(sel.orientation == this.orientation
		 && sel.coords.contains(structAxis)
		 && elementFor(sel.coords).containsInEdgeArea(sel))


	override def selectEdgeLabel(parent: Map[Axis,Int], labelOrientation: Orientation, plane: Orientation, end: End, hintSel: SingleGridSelection): Selectable = {
		if (labelOrientation == plane)
			selectSideEdgeLabel(parent, labelOrientation, end, hintSel)
		else if (end == First)
			selectNearEdgeLabel(parent, labelOrientation, hintSel)
		else
			selectFarEdgeLabel(parent, labelOrientation, hintSel)
	}


	/**
	 * Selects an item on the ‘long’ (longitudinal), near edge.
	 */
	def selectNearEdgeLabel(parent: Map[Axis,Int], labelOrientation: Orientation, hintSel: SingleGridSelection) =
		super.selectEdgeLabel(parent, labelOrientation, labelOrientation, First, hintSel) orElse {
			// If we get here, it means that we don’t actually have any axes on this orientation
			assert(axes(labelOrientation).isEmpty)
			// And it means that it’s not ourStructAxis side:
			assert(labelOrientation != this.orientation)
			// => There are no child labels:
			NullSelection
		}


	/**
	 * Selects an item on the ‘long’ (longitudinal), far edge.
	 */
	def selectFarEdgeLabel(parent: Map[Axis,Int], labelOrientation: Orientation, hintSel: SingleGridSelection) = {
		val childSelection =
			if (labelOrientation == this.orientation){
				val coords = parent ++ hintCoords(axes(labelOrientation), hintSel.coords)
				elementFor(coords)
					.selectEdgeLabel(coords, labelOrientation, labelOrientation.opposite, Last, hintSel)
			}else
				NullSelection

		childSelection orElse
			super.selectEdgeLabel(parent, labelOrientation, labelOrientation.opposite, Last, hintSel)
	}


	/**
	 * Selects one of the items on one of the transverse edges.
	 */
	def selectSideEdgeLabel(parent: Map[Axis,Int], labelOrientation: Orientation, end: End, hintSel: SingleGridSelection) = {
		NullSelection // TODO
	}


	def moveByOne(sel: SingleGridSelection, o: Orientation, d: Direction): Selectable = sel match {
		case label: OneLabel if this containsInEdgeArea label =>
			val isTransverse = (label.orientation == o.opposite)
			if (isPrimaryAxis(label.orientation)){
				lazy val el = elementFor(sel.coords)
				
				if (label.block == this) // It’s one of our own axes’ labels:
					moveOwnLabelByOne(label, o, d) orElse {
						// If moving from our axes to child axes:
						if (isTransverse && d.isForward)
							el.selectEdgeLabel(sel.coords, label.orientation, o, First, sel)
						else
							NullSelection
					}
					
				else // It's one of our 'combined' child labels:
					el.moveByOne(label, o, d) orElse {
						// If moving from child axes to our axes:
						if (isTransverse)
							if (d.isBack)
								super.selectEdgeLabel(label.parentCoords, label.orientation, o, Last, sel)
							else
								NullSelection // Move off label area

						else // is longitudianal
							nextOnAxes(axes(o), label.parentCoords, d) match {
								case Some(c) => new OneLabel(this, o, c)
								case None => NullSelection
							}
					}

			}else{ // secondary axis
				moveOwnLabelByOne(label, o, d)
			}

		case _ => // Is NOT a label in my edge area (so either a data cell, or a label which is not in my edge area)
			val el = elementFor(sel.coords)
			el.moveByOne(sel, o, d) orElse {
				sel match {
					case label: OneLabel if el.containsInEdgeArea(label) =>
						assert(label.orientation != this.orientation) // Otherwise, it would be in my edge area.

						val isTransverse = label.orientation == o.opposite
						if (isTransverse){
							if (d.isForward)
								el.selectEdgeChild(sel.coords, o, First, sel)
							else
								nextOnAxes(axes(o), sel.coords, d) match {
									case Some(c) =>
										el.selectEdgeChild(c, o, Last, sel)
									case None => NullSelection
								}

						}else // Moving longitutinal to child label
							nextOnAxes(axes(o), sel.coords, d) match {
								case Some(c) =>
									assert(el == elementFor(c))
									el.selectEdgeLabel(c, o, o, if (d.isForward) First else Last, sel)
								case None => NullSelection
							}

					case _ => // Child cell, (NOT immediate child's label area).
						if (o == this.orientation) { // across child labels
							val parentCoords = sel.coords -- el.axes(o)
							if (d.isForward)
								nextOnAxes(axes(o), parentCoords, d) match {
									case Some(c) =>
										val el = elementFor(c)
										el.selectEdgeLabel(c, o.opposite, o, First, sel) orElse {
											el.selectEdgeChild(c, o, First, sel)
										}

									case None => NullSelection
								}
							else
								el.selectEdgeLabel(parentCoords, o.opposite, o, Last, sel) orElse {
									nextOnAxes(axes(o), parentCoords, d) match {
										case Some(c) =>
											val el = elementFor(c)
											el.selectEdgeChild(c, o, Last, sel)

										case None => NullSelection
									}
								}
						} else
							nextOnAxes(axes(o), sel.coords, d) match {
								case Some(c) =>
									val el = elementFor(c)
									el.selectEdgeChild(c, o, if (d.isForward) First else Last, sel)

								case None => NullSelection
							}
				}
			}
	}


	def selectEdgeChild(context: Map[Axis,Int], plane: Orientation, end: End, hintSel: SingleGridSelection): Selectable = {
		val myFirst = context ++ (axes(plane) map (ax => (ax, end.of(ax))))
		val myOrth = Map.empty ++ hintCoords(axes(plane.opposite), hintSel.coords)
		val myContext = myFirst ++ myOrth
		val planeParallelToStruct = (plane == this.orientation)
		
		val el = if (planeParallelToStruct) end.of(elements) else elementFor(myOrth)

		val maybeLabel = if (planeParallelToStruct && end.isFirst)
				el.selectEdgeLabel(myContext, plane.opposite, plane, First, hintSel) 
			else
				NullSelection

		maybeLabel orElse
			el.selectEdgeChild(myContext, plane, end, hintSel)
	}

	
	/*------------------------------------------------------------------------*/
	// SIZING

	def breadthOfCell(o: Orientation, c: Map[Axis, Int]): Int = {
		if (isPrimaryAxis(o))
			elementFor(c).outerBreadth(o)
		else
			maxElementDepth
	}

	/**
	 * Returns the pixel range of the breadth (in the orientation ‘o’) of the cell(s)
	 * given by <var>coords</var>.
	 */
	def breadthCellBounds(offset: Int, o: Orientation, coords: Map[Axis,Int]): Range = {
		val i = cellIndexOf(o, coords)
		val b = offset + starts(o)(i)
		val el = elementFor(coords)
		// If we're looking at orientation transverse to structAxis, all sub-block
		// headers are subsumed inside this block's own logitudinal header:
		val h = if (o == this.orientation) el.firstHeader(o) else 0
		el.breadthCellBounds(b + h, o, coords)
	}

	
	def breadthOwnLabelBounds(offset: Int, o: Orientation, coords: Map[Axis,Int], num: Int) = {
		val i = cellIndexOf(o, coords)
		new Range(
			offset + starts(o)(i), 
			offset + ends(o)(i + num - 1), 1)
	}


	def childLabelBounds(offset: Point, lab: OneLabel): Rectangle = {
		val (b0, d0) = this.orientation.breadthDepth(offset)
		val el = elementFor(lab.coords)
		val bOffset = el.firstHeader(orientation) +
					starts(orientation)(cellIndexOf(orientation, lab.coords))
					
		val dOffset =
			if (this containsInEdgeArea lab) {
				assert(this.orientation == lab.orientation)
				0

			} else {
				val orthogonal = orientation.opposite
				starts(orthogonal)(cellIndexOf(orthogonal, lab.coords))
			}

		val childOffset = orientation.newPoint(
					b0 + bOffset,
					d0 + dOffset)
		el.labelBounds(childOffset, lab)
	}

	/*------------------------------------------------------------------------*/
	// OTHER

	def arrayTable(coords: Map[Axis,Int]) =
		elements(coords(structAxis))
			.arrayTable(coords)


	def elementFor(coords: Map[Axis,Int]) =
		elements(coords(structAxis))


	/*------------------------------------------------------------------------*/

	override def toString = "ArrayBlock"
}
