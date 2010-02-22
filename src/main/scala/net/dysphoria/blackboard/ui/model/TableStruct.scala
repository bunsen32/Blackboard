/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.model

import scala.collection.mutable
import org.eclipse.swt.graphics._
import net.dysphoria.blackboard._
import gfx._
import ui._
import ui.selection._

class TableStruct(val structAxis: StructAxis) extends TablePart {
	var orientation: Orientation = Horizontal
	var elements = new mutable.ArrayBuffer[TablePart]
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
		endsX = new Array[Int]((1 /: xAxes)(_ * visibleLength(_)))
		endsY = new Array[Int]((1 /: yAxes)(_ * visibleLength(_)))

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

	def headerDepth(o: Orientation, axes: Seq[Axis], element: Option[TablePart]): Int = {
		if (!axes.isEmpty){
			val remainingAxes = axes.drop(1)
			axes.first match {
				case a: ArrayAxis =>
					val header = headerDepth(o, remainingAxes, element)
					val maxLabelDepth = (0 /: visibleRange(a))((max, i)=>
						Math.max(max, preferredLabelDepth(o, a, i)))
					header + maxLabelDepth

				case s: StructAxis =>
					assert(isPrimaryAxis(o), "StructAxes only allowed on primary dimension (by definition).")
					assert(s == structAxis, "Must be the correct instance of StructAxis")
					assert(element.isEmpty, "Must not have already found a StructAxis")
					val labelChildDepths = new Array[Int](visibleLength(s))
					val maxDepth = (0 /: visibleRange(s))((maxHead, i) => {
						val structLabelDepth = preferredLabelDepth(o, s, i)
						val elHead = headerDepth(o, remainingAxes, Some(elements(i)))
						labelChildDepths(i) = elHead
						Math.max(maxHead, elHead + structLabelDepth)
					})
					labelDepths = labelChildDepths // Ugly hack to avoid allocation. (Modifies array in-place.)
					for (i <- visibleRange(s))
						labelDepths(i) = maxDepth - labelChildDepths(i)

					maxDepth

				case _=> error("Don't recognise that kind of Axis")
			}
			
		}else {
			if (isPrimaryAxis(o))
				element.getOrElse(error("TableStruct primary axis must contain a StructAxis!"))
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
		el.renderLabels(gfx, dataOrigin, orientation.other, indices)
	}

	override type Instance = TableStructInstance

	def instance(container: ContainingType, coords: Map[Axis, Int]) = new TableStructInstance(container, coords)

	class TableStructInstance(val container: ContainingType, val coords: Map[Axis, Int]) extends TablePartInstance {
		override def model = TableStruct.this
		type ComponentType = TablePart#TablePartInstance

		def cell(cellCoords: Map[Axis, Int])  = {
			val el = model.elementFor(cellCoords)
			el.instance(this, cellCoords)
		}

		/*------------------------------------------------------------------------*/
		// HIT-TESTING

		def hitTestCell(cellCoords: Map[Axis,Int], rel: Point) = {
			val part = cell(cellCoords)
			val innerHead = part.firstHeader(orientation)
			val (b, d) = orientation.breadthDepth(rel)
			if (b < innerHead) {// header area
				part.hitTestLabels(orientation.other, d, b - innerHead)

			} else {
				val dataRel = orientation.newPoint(b - innerHead, d)
				part.hitTestCells(dataRel) orElse part
			}
		}

		/*------------------------------------------------------------------------*/
		// NAVIGATION (self)

		def edgeCell(edge: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection] = {
			val plane = edge.orientation
			val end = edge.end
			val part = cell(
				coords
				++ (axes(plane) map (endPair(_, end)))
				++ hintCoords(axes(plane.other), hint.coords))

			edgeOfChild(part, edge)
		}

		/*------------------------------------------------------------------------*/
		// NAVIGATION (component elements)

		def oneBeyond(child: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection] =
			adjacentChild(child, direction) flatMap (edgeOfChild(_, direction.opposite)) orElse
				container.oneBeyondContent(this, direction)

		def oneBeyondContent(child: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints) =
			if (hasLabelsOnEdgeOf(child, direction)) {
				val axisOrientation = direction.orientation.other
				child.edgeLabel(axisOrientation, direction.opposite)
			}else
				oneBeyond(child, direction)

		def edgeOfChild(child: ComponentType, direction: CompassPosition)(implicit hints: SelectionHints): Option[SingleGridSelection] =
			if (hasLabelsOnEdgeOf(child, direction)) {
				val axisOrientation = direction.orientation.other
				child.edgeLabel(axisOrientation, direction)
			} else
				child.edgeCell(direction)

		def hasLabelsOnEdgeOf(child: ComponentType, direction: CompassPosition) =
			direction.end.isFirst && {
				val potentialAxisOrientation = direction.orientation.other
				val childHasAxes = !child.model.axes(potentialAxisOrientation).isEmpty
				val thisBlockSubsumesChildLabels = containsLabelsInEdgeArea(child, potentialAxisOrientation)
				(childHasAxes && !thisBlockSubsumesChildLabels)
			}

		def oneBeyondLabelArea(childPartLabel: UnnestedLabelInstance, direction: CompassPosition)(implicit hint: SelectionHints) = {
			val childPart = childPartLabel.containingPart
			assert(childPart.container == this)
			if (direction.orientation == childPartLabel.transverse) // direction transverse => long edge
				if (direction.forwardBack.isForward)
					childPart.edgeCell(direction.opposite)
				else
					oneBeyond(childPart, direction)
			else { // direction longitudinal => short edge
				val orientation = childPartLabel.orientation ensuring(_ == direction.orientation)
				adjacentChild(childPart, direction) flatMap {newChild =>
					val labelEdge = CompassPosition(orientation, First)
					if (hasLabelsOnEdgeOf(newChild, labelEdge))
						newChild.edgeLabel(orientation, direction)
					else
						None
				} orElse
					container.oneBeyond(this, direction)
			}
		}

	}

	def edgeLabel(container: LabelContainer, whichLabels: Orientation, edge: CompassPosition)(implicit hint: SelectionHints) = {
		val optionalOwnLabel = ownedEdgeLabel(container, whichLabels, edge)
		optionalOwnLabel flatMap { ownLabel: LabelInstance =>
			lazy val el = elementFor(ownLabel.coords)
			lazy val elAxes = el.axes(whichLabels)
			if (ownLabel.orientation == this.orientation)
				if (ownLabel.isTransverse(edge.orientation)) // long edge
					if (edge.end.isFirst)
						None
					else
						el.edgeLabel(ownLabel, whichLabels, edge)
				else // short edge
					if (ownLabel.isDeepest && !elAxes.isEmpty && hint.coords.contains(elAxes.first)) {
						println("descending...")
						el.edgeLabel(ownLabel, whichLabels, edge)
					}else
						None
			else
				None
		} orElse
			optionalOwnLabel
	}

	def firstChildLabelOf(ownLabel: LabelInstance)(implicit hint: SelectionHints) = {
		assert(ownLabel.ownerPartModel == this)
		assert(ownLabel.isDeepest)

		val axisOrientation = ownLabel.orientation
		lazy val el = elementFor(ownLabel.coords)
		lazy val elAxes = el.axes(axisOrientation)
		if (axisOrientation == this.orientation){ // i.e., structAxis orientation
			val edge = CompassPosition(axisOrientation.other, First)
			el.edgeLabel(ownLabel, axisOrientation, edge)
		}else
			None
	}


	def hitTestChildLabels(parentLabel: LabelInstance, b: Int, d: Int) = {
		val o = parentLabel.orientation
		if (isPrimaryAxis(o)){
			val el = elementFor(parentLabel.coords)
			el.hitTestLabels(parentLabel, o, b - el.firstHeader(o), d)
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
			var axesCoords: Map[Axis,Int] = Map.empty
			var n = n0
			var i = i0
			for(ax <- axes(o)) {
				val width = n / visibleLength(ax)
				axesCoords += ((ax, i / width))
				i %= width
				n = width
			}
			Some((axesCoords, remainder))
		}else
			None
	}

	def containsLabelsInEdgeArea(child: TablePart, labelOrientation: Orientation) =
		(labelOrientation == this.orientation && !child.axes(labelOrientation).isEmpty)

	def containsInEdgeArea(label: LabelInstance) =
		(label.ownerPartModel == this) ||
		(label.orientation == this.orientation
		 && label.coords.contains(structAxis)
		 && elementFor(label.coords).containsInEdgeArea(label))

	
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
		// headers are subsumed inside this block's own longitudinal header:
		val h = if (o == this.orientation) el.firstHeader(o) else 0
		el.breadthCellBounds(b + h, o, coords)
	}

	
	def breadthOwnLabelBounds(offset: Int, o: Orientation, coords: Map[Axis,Int], num: Int) = {
		val i = cellIndexOf(o, coords)
		new Range(
			offset + starts(o)(i), 
			offset + ends(o)(i + num - 1), 1)
	}


	def childLabelBounds(offset: Point, lab: LabelInstance): Rectangle = {
		val (b0, d0) = this.orientation.breadthDepth(offset)
		val el = elementFor(lab.coords)
		val bOffset = el.firstHeader(orientation) +
					starts(orientation)(cellIndexOf(orientation, lab.coords))
					
		val dOffset =
			if (this containsInEdgeArea lab) {
				assert(this.orientation == lab.orientation)
				0

			} else {
				val orthogonal = orientation.other
				starts(orthogonal)(cellIndexOf(orthogonal, lab.coords))
			}

		val childOffset = orientation.newPoint(
					b0 + bOffset,
					d0 + dOffset)
		el.labelBounds(childOffset, lab)
	}

	/*------------------------------------------------------------------------*/
	// OTHER

	def elementFor(coords: Map[Axis,Int]) =
		elements(coords(structAxis))


	/*------------------------------------------------------------------------*/

	override def toString = "TableArrayData"
}
