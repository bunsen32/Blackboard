/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.model

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._

import net.dysphoria.blackboard
import blackboard.gfx._
import blackboard.ui._
import blackboard.ui.selection._

abstract class TablePart {
	val genericCellHeight = 19*256 // Need to get rid of these at some point.
	val genericCellWidth = 50*256 // Will be replaced by the CSS styles.

	var xAxes: Seq[Axis] = Nil
	var yAxes: Seq[Axis] = Nil

	var topHeader: Int = 0
	var leftHeader: Int = 0
	var bottomHeader = 0 // For the time being we ignore bottom headers
	var rightHeader = 0 // For the time being we ignore right headers
	var innerSize: Point = null
	var outerSize: Point = null

	private val defaultLabelStyle = new CellStyle {
		marginLeft = 2
		marginRight= 2
		backgroundColor = new RGB(78, 105, 180)
		color = new RGB(0, 0, 0)
		textAlign = TextAlignCenter
		fontStyle = SWT.BOLD
	}
	private val voidLabelStyle = new CellStyle {
		marginLeft = 2
		marginRight= 2
		backgroundColor = new RGB(110, 110, 160)
		color = new RGB(0, 0, 0)
		textAlign = TextAlignCenter
		fontStyle = SWT.BOLD | SWT.ITALIC
	}

	def labelStyle(axis: Axis, index: Int) = defaultLabelStyle

	def preferredLabelDepth(labelOrientation: Orientation, a: Axis, i: Int) = a match {
		case s: StructAxis if !s.visible(i) => 0
		case _ => if (labelOrientation.isX) xLabelHeight(a, i) else yLabelWidth(a, i)
	}
			

	def labelDepth(labelOrientation: Orientation, a: Axis, i: Int): Int

	def xLabelHeight(a: Axis, i: Int) = genericCellHeight
	def yLabelWidth(a: Axis, i: Int) = genericCellWidth

	object axes {
		def apply(o:Orientation) = if (o.isX) xAxes else yAxes
		def update(o:Orientation, as: Seq[Axis]) = if (o.isX) xAxes = as else yAxes = as
	}

	def innerBreadth(o:Orientation) = if (o.isX) innerSize.x else innerSize.y
	def innerDepth(o:Orientation) = if (o.isY) innerSize.x else innerSize.y
	def outerBreadth(o:Orientation) = if (o.isX) innerSize.x + leftHeader + rightHeader else  innerSize.y + topHeader + bottomHeader
	def nearHeader(o:Orientation) = if (o.isX) topHeader else leftHeader
	def farHeader(o:Orientation) = if (o.isX) bottomHeader else rightHeader
	def firstHeader(o:Orientation) = if (o.isX) leftHeader else topHeader
	def lastHeader(o: Orientation) = if (o.isX) rightHeader else bottomHeader

	final def computeSize {
		computeInnerSizeAndHeaders
	}

	// Should populate header sizes, and inner and outer sizes and labelDepth arrays
	def computeInnerSizeAndHeaders

	def isPrimaryAxis(o: Orientation) = false

	
	/*------------------------------------------------------------------------*/
	// RENDERING

	def renderCells(gfx: DrawingContext, origin: Point, coords: Map[Axis, Int]) {

		def drawRules(o:Orientation, b0: Int, d0: Int, depth: Int, axes: Seq[Axis], coords: Map[Axis, Int]): Int = {
			iterateAxis(b0, axes, (b0, axis, i, remainingAxes)=>{
				val updatedCoords = coords.update(axis, i)
				val breadth = drawRules(o, b0, d0, depth, remainingAxes, updatedCoords)
				if (i != 0) drawSeparator(gfx, axis, o.other, d0, b0, depth)
				breadth
			},{
				breadthOfCell(o, coords)
			})
		}

		iterateValues(origin.y, yAxes, coords, (y, rowCoords)=>{
			val h = breadthOfCell(Vertical, rowCoords)
			iterateValues(origin.x, xAxes, rowCoords, (x, cellCoords)=>{
				val w = breadthOfCell(Horizontal, cellCoords)
				val bounds = new Rectangle(x, y, w, h)
				renderCell(gfx, bounds, cellCoords)
				w
			})
			h
		})

		drawRules(Horizontal, origin.x, origin.y, innerSize.y, xAxes, coords)
		drawRules(Vertical, origin.y, origin.x, innerSize.x, yAxes, coords)
	}


	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int])


	def renderLabels(gfx: DrawingContext, dataOrigin: Point, o:Orientation, coords: Map[Axis, Int]){
		
		def ownLabels(b0: Int, d0: Int, availableDepth: Int,
						 axes: Seq[Axis], coords: Map[Axis, Int]): Int = {

			iterateAxis(b0, axes, (b0, axis, i, remainingAxes)=>{
					val updatedCoords = coords.update(axis, i)
					val depth = labelDepth(o, axis, i)
					val breadth = ownLabels(b0, d0, availableDepth - depth, remainingAxes, updatedCoords)
					var d1 = d0 - availableDepth
					val lBounds = o.newRectangle(b0, d1, breadth, depth)
					renderHeaderLabel(gfx, lBounds, axis, i, updatedCoords)
					if (i != 0) drawSeparator(gfx, axis, o.other, d1, b0, availableDepth)
						
					breadth
				}, {
					renderChildLabels(gfx, o, b0, d0, availableDepth, coords)
				})
		}

		ownLabels(o.breadth(dataOrigin), o.depth(dataOrigin), nearHeader(o), axes(o), coords)
	}


	def renderChildLabels(gfx: DrawingContext, o: Orientation, b0: Int, d0: Int, availableDepth: Int,
						  coords: Map[Axis, Int]): Int = {

		breadthOfCell(o, coords)
	}

	type ContainingType = TableBuildingBlock{type ComponentType=TablePart#TablePartInstance}

	type Instance <: TablePartInstance

	def instance(container: ContainingType, coords: Map[Axis, Int]): TablePartInstance

	abstract class TablePartInstance extends TableSubItemSelection with TableBuildingBlock with LabelContainer {
		def container: ContainingType
		val coords: Map[Axis, Int]
		def model = TablePart.this
		type ComponentType <: TableSubItemSelection

		def unambiguousCoords = coords

		def table: Table#Instance = container match {
			case t: Table#Instance => t
			case part: TablePart#TablePartInstance => part.table
		}

		// Arguably these should be type errors rather than runtime errors:
		def bounds = error("TablePart does not have bounds in and of itself")
		def position = error("TablePart does not have a position in and of itself")

		override def equals(other: Any) = other match {
			case partInstance: TablePartInstance =>
				partInstance.model == this.model &&
				partInstance.coords == this.coords

			case _ => false
		}

		override def hashCode =
			(this.model.hashCode * 41) ^ this.coords.hashCode

		/**
		 * Returns an object representing the given cell. For TableStructs this is a further TablePart;
		 * for TableArrays, this is a CellInstance of some kind.
		 */
		def cell(cellCoords: Map[Axis, Int]): ComponentType

		/*------------------------------------------------------------------------*/
		// HIT TESTING

		def hitTestCells(p: Point): Selectable = {
			val hitX = hitTestAxis(Horizontal, p.x)
			val hitY = hitTestAxis(Vertical, p.y)
			(hitX, hitY) match {
				case (Some((cx, remainderX)), Some((cy, remainderY))) =>
					hitTestCell(coords++cx++cy, new Point(remainderX, remainderY))
				case _=> NullSelection
			}
		}

		final def hitTestLabels(o: Orientation, b: Int, d0: Int): Selectable =
			model.hitTestLabels(this, o, b, d0)

		protected def hitTestCell(totalCoords: Map[Axis,Int], relative: Point): Selectable

		/*------------------------------------------------------------------------*/
		// NAVIGATION (self)

		def edgeCell(edge: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection]

		final def edgeLabel(whichLabels: Orientation, edge: CompassPosition)(implicit hint: SelectionHints) = 
			model.edgeLabel(this, whichLabels, edge)

		/*------------------------------------------------------------------------*/
		// NAVIGATION (component elements)

		def adjacentChild(child: ComponentType, direction: CompassPosition): Option[ComponentType] =
			nextOnAxes(axes(direction.orientation), child.unambiguousCoords, direction.forwardBack) map (cell(_))

	}

	def edgeLabel(container: LabelContainer, whichLabels: Orientation, edge: CompassPosition)(implicit hint: SelectionHints): Option[LabelInstance]

	protected def ownedEdgeLabel(container: LabelContainer, whichLabels: Orientation, edge: CompassPosition)(implicit hint: SelectionHints) = {
		val theAxes = axes(whichLabels)
		if (theAxes.isEmpty)
			None
		else {
			val firstAxis = theAxes.first
			val labelCoords = container.unambiguousCoords ++ (
				if (whichLabels == edge.orientation){ // 'Short' (transverse) edge of label block.
					theAxes.takeWhile(ax => (ax == firstAxis) || (hint.coords.contains(ax))) map
						   (endPair(_, edge.end))

				}else{ // 'Long' (longitudinal) edge of label block
					val axes = if (edge.end.isFirst) Seq(firstAxis) else theAxes
					hintCoords(axes, hint.coords)
				})
			Some(LabelInstance(container, this, whichLabels, labelCoords))
		}
	}

	def firstChildLabelOf(container: LabelInstance)(implicit hint: SelectionHints): Option[LabelInstance]

	/*------------------------------------------------------------------------*/
	// HIT TESTING

	def hitTestLabels(container: LabelContainer, o: Orientation, b: Int, d0: Int): Selectable =
		hitTestAxis(o, b) match {
			case None => NullSelection

			case Some((axisCoords, deltaB)) =>
				def searchLabels(allCoords: Map[Axis,Int], remainingAxes: Seq[Axis], d: Int): Selectable = {
					val axis = remainingAxes.first
					val remainingAxesMinusOne = remainingAxes.drop(1)
					val i = axisCoords(axis)
					val coordsSoFar = allCoords + (axis -> i)
					val axisDepth = labelDepth(o, axis, i)
					if (d >= axisDepth && !remainingAxesMinusOne.isEmpty)
						searchLabels(coordsSoFar, remainingAxesMinusOne, d - axisDepth)

					else{
						val label = LabelInstance(container, this, o, coordsSoFar)
						if (d >= axisDepth)
							hitTestChildLabels(label, deltaB, d0)
						else
							label
					}
				}

				if (axes(o) isEmpty) {
					// Assumes that if axes(o) is empty, we do not have child axes here either.
					NullSelection
				} else
					searchLabels(container.unambiguousCoords, axes(o), d0 + nearHeader(o))
		}


	def hitTestChildLabels(parent: LabelInstance, b: Int, d: Int): Selectable


	def hitTestAxis(o: Orientation, b: Int): Option[(Map[Axis,Int], Int)]


	/**
	 * Returns true iff this label is in my own label area. (This includes a struct’s
	 * child-blocks’ labels which have been promoted upwards.)
	 */
	def containsInEdgeArea(sel: LabelInstance): Boolean

	def hintCoords(axes: Seq[Axis], hints: Map[Axis,Int]) =
		axes.map(ax => (ax, hints.getOrElse(ax, 0)))


	/*------------------------------------------------------------------------*/
	// SIZING

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]): Int


	/**
	 * Returns the pixel range of the breadth (in the orientation ‘o’) of the cell(s)
	 * given by <var>coords</var>.
	 */
	def breadthCellBounds(offset: Int, o: Orientation, coords: Map[Axis,Int]): Range


	def labelBounds(dataOrigin: Point, label: LabelInstance): Rectangle = {
		if (label.ownerPartModel == this) {
			val o = label.orientation
			val longiRange = breadthOwnLabelBounds(o.breadth(dataOrigin), label)
			val transRange = depthOwnLabelBounds(o.depth(dataOrigin), label)
			o.newRectangle(longiRange.start, transRange.start, longiRange.length, transRange.length)

		}else{
			childLabelBounds(dataOrigin, label)
		}
	}

	def childLabelBounds(dataOrigin: Point, lab: LabelInstance): Rectangle

	def breadthOwnLabelBounds(offset: Int, label: LabelInstance): Range = {
		assert(label.ownerPartModel == this)
		val o = label.orientation; val coords = label.coords
		val num = cellCountOf(o, coords)
		breadthOwnLabelBounds(offset, o, coords, num)
	}

	def breadthOwnLabelBounds(offset: Int, o: Orientation, coords: Map[Axis,Int], num: Int): Range


	def depthOwnLabelBounds(dataOrigin: Int, label: LabelInstance): Range = {
		assert(label.ownerPartModel == this)
		val o = label.orientation
		val coords = label.coords
		val allDepths =	for(ax <- axes(o).take(label.axisIndex + 1))
			yield labelDepth(o, ax, coords(ax))

		rangeOfLast(dataOrigin - nearHeader(o), allDepths)
	}


	/*------------------------------------------------------------------------*/
	// ITERATION

	def iterateValues(origin: Int, axes: Seq[Axis], coords: Map[Axis, Int],
					perValue: (Int, Map[Axis,Int])=>Int): Int = {

		iterateAxis(origin, axes, (b0, axis, i, remainingAxes)=>{
				val updatedCoords = coords.update(axis, i)
				iterateValues(b0, remainingAxes, updatedCoords, perValue)
			}, {
				perValue(origin, coords)
			})
	}


	def iterateAxis(origin: Int, axes: Seq[Axis], 
				    perLabel: (Int, Axis, Int, Seq[Axis])=>Int,
					bottom: =>Int): Int = {

		if (!axes.isEmpty){
			val axis = axes(0)
			val remainingAxes = axes.drop(1)
			var offset = 0
			for(i <- visibleRange(axis)){
				val d = perLabel(origin + offset, axis, i, remainingAxes)
				offset += d
			}
			offset

		}else{
			bottom
		}
	}


	def nextOnAxes(blockAxes: Seq[Axis], start: Map[Axis,Int], d: Direction): Option[Map[Axis,Int]] = {
		val delta = d.delta
		def next(axes: Seq[Axis], old: Map[Axis, Int]): Option[Map[Axis,Int]] = {
			if (axes.isEmpty)
				None
			else {
				val axis = axes.first
				val p = start(axis)
				val newP = p + delta
				if (newP < 0)
					// Before start of axis. Wrap around and move onto next axis:
					next(axes.drop(1), old + (axis -> axis.last))

				else if (newP < visibleLength(axis))
					// Within range. Accept new value:
					Some(old + (axis -> newP))

				else
					// After end of axis. Wrap around and move onto next axis:
					next(axes.drop(1), old + (axis -> axis.first))
			}
		}
		// We increment least-significant axis first, moving onto more-
		// significant axes if we hit the end of the range.
		next(blockAxes.reverse, start)
	}

	
	def first(o: Orientation): Map[Axis, Int] = axesEnd(o, First)

	def last(o: Orientation): Map[Axis, Int] = axesEnd(o, Last)

	def axesEnd(o:Orientation, anEnd: End): Map[Axis, Int] = (
		Map.empty
		++ (this.axes(o).map(endPair(_, anEnd)))
		++ (this match {
				case s: TableStruct if s.orientation == o =>
					s.elements(endOf(s.structAxis, anEnd)).axesEnd(o, anEnd)
				case _ => Nil
			}))


	/**
	 * Given some coordinates, returns how many cells that is in the given
	 * orientation. Note that if there are missing coordinates, they will be
	 * assumed to be zero.
	 *
	 * Note that other things probably assume that if you have one missing coord,
	 * all subsequent coords are missing too!
	 */
	def cellIndexOf(o: Orientation, coords: Map[Axis, Int]): Int = {
		var i = 0
		var hasFurtherCoords = true
		for (ax <- axes(o)) {
			hasFurtherCoords &&= coords.contains(ax)
			val c = if (hasFurtherCoords) coords(ax) else 0
			i = i * visibleLength(ax) + c
		}
		i
	}


	/**
	 * Returns the number of cells contained within the specified (label) coordinates.
	 * Inludes 'blank' cell for empty axes.
	 */
	def cellCountOf(o: Orientation, coords: Map[Axis, Int]): Int = {
		val axesWithValues = axes(o).dropWhile(coords.contains(_))
		(1 /: axesWithValues)((product, ax) => product * visibleLength(ax))
	}

	//def arrayTable(coords: Map[Axis,Int]): DataArray


	/*------------------------------------------------------------------------*/
	// COORDINATES

	def coordinatesMatch(required: Map[Axis,Int], actual: Map[Axis, Int]) =
		required.forall(kv => {val (k, v) = kv; actual.get(k)==Some(v)})


	def lastAxisIn(axes: Seq[Axis], coords: Map[Axis, Int]) =
		axes.findIndexOf(!coords.contains(_)) match {
			case -1 => axes.length - 1
			case n => n - 1
		}

	def rangeOfLast(offset: Int, numbers: Seq[Int]) = {
		var i0 = offset
		var i1 = offset
		for(n <- numbers){
			i0 = i1
			i1 = i0 + n
		}
		new Range(i0, i1, 1)
	}

	def endPair(ax: Axis, end: End) = (ax, endOf(ax, end))

	def endOf(ax: Axis, end: End) = if (end.isFirst) 0 else visibleLength(ax) - 1

	def visibleRange(ax: Axis) = ax match {
		case a: ArrayAxis if a.length == 0 => 0 until 1
		case _ => ax.range
	}

	def visibleLength(ax: Axis) = ax match {
		case a: ArrayAxis => a.length max 1
		case _ => ax.length
	}

	
	/*------------------------------------------------------------------------*/
	// GFX UTILITY METHODS


	def renderHeaderLabel(gfx: DrawingContext, bounds: Rectangle, ax: Axis, index: Int, thisCoords: Map[Axis,Int]) {
		def isThisLabel(label: LabelInstance) = {
			label.ownerPartModel == this &&
			label.coords == thisCoords
		}
		def isInRange(range: LabelRange) = {
			(range.ownerPartModel == this) &&
			(range.axis == ax) &&
			(range.range contains index) &&
			(coordinatesMatch(range.unambiguousCoords, thisCoords))
		}

		val withinData = ax.range.contains(index)
		val selected = gfx.ui.selection match {
			case lab: LabelInstance => if (isThisLabel(lab)) 2 else 0
				
			case labs: LabelRange => if (isInRange(labs)) 2 else 0

			case cell: AbstractCellInstance =>
				@inline def matches(l: Option[LabelInstance]) = l match {
					case None => false
					case Some(lab) => isThisLabel(lab)
				}
				if (matches(cell.hLabel) || matches(cell.vLabel)) 1 else 0

			case _ => 0
		}
		if (withinData)
			gfx.renderBasicCell(labelStyle(ax, index), bounds,
							ax.label(index), selected, ax.greyed(index))
		else
			gfx.renderBasicCell(voidLabelStyle, bounds,
							"(none)", selected, false)
	}


	def drawSeparator(gfx: DrawingContext, d: Axis, o: Orientation, b0: Int, d0: Int, length: Int){
		d.interItemLine match {
			case None => ;// No line
			case Some(l) => {
				gfx.setLine(l)
				val off = (l.thickness / 2).toInt
				val dEdge = d0 - off // Centre the line on pixel boundary.
				o.choose(gfx.gc.drawLine(b0, dEdge, b0 + length, dEdge),
						 gfx.gc.drawLine(dEdge, b0, dEdge, b0 + length))
			}
		}
	}
}


object TablePart {
	implicit def instanceToClass(instance: TablePart#TablePartInstance) = instance.model
}