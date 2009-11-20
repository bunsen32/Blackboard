/*
 * Block.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import blackboard.gfx._
import ui.selection._

abstract class TableBlock {
	val genericCellHeight = 19 // Need to get rid of these at some point.
	val genericCellWidth = 50 // Will be replaced by the CSS styles.

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
		backgroundColor = new RGB(90, 90, 180)
		color = new RGB(0, 0, 0)
		textAlign = TextAlignCenter
		fontStyle = SWT.BOLD
	}

	def labelStyle(axis: Axis, index: Int) = defaultLabelStyle

	def preferredLabelDepth(labelOrientation: Orientation, a: Axis, i: Int) =
		if (a.label(i) == "")
			0
		else
			if (labelOrientation.isX) xLabelHeight(a, i) else yLabelWidth(a, i)

	def labelDepth(labelOrientation: Orientation, a: Axis, i: Int): Int

	def xLabelHeight(a: Axis, i: Int) = genericCellHeight
	def yLabelWidth(a: Axis, i: Int) = genericCellWidth

	object axes {
		def apply(o:Orientation) = if (o.isX) xAxes else yAxes
		def update(o:Orientation, as: Seq[Axis]) = if (o.isX) xAxes = as else yAxes = as
	}

	def innerBreadth(o:Orientation) = if (o.isX) innerSize.x else innerSize.y
	def innerDepth(o:Orientation) = if (o.isY) innerSize.x else innerSize.y
	def outerBreadth(o:Orientation) = if (o.isX) outerSize.x else outerSize.y
	def outerDepth(o:Orientation) = if (o.isY) outerSize.x else outerSize.y
	def nearHeader(o:Orientation) = if (o.isX) topHeader else leftHeader
	def farHeader(o:Orientation) = if (o.isX) bottomHeader else rightHeader
	def firstHeader(o:Orientation) = if (o.isX) leftHeader else topHeader
	def lastHeader(o: Orientation) = if (o.isX) rightHeader else bottomHeader

	final def computeSize {
		computeInnerSizeAndHeaders
		outerSize = new Point(innerSize.x + leftHeader + rightHeader,
							  innerSize.y + topHeader + bottomHeader)
	}

	// Should populate header sizes, and inner and outer sizes and labelDepth arrays
	def computeInnerSizeAndHeaders

	def isPrimaryAxis(o: Orientation) = false

	
	/*------------------------------------------------------------------------*/
	// RENDERING

	def renderCells(gfx: DrawingContext, origin: Point, coords: Map[Axis, Int]) {

		def drawRules(o:Orientation, b0: Int, d0: Int, depth: Int, axes: Seq[Axis], coords: Map[Axis, Int]): Int = {
			iterateAxis(b0, axes, coords, (b0, axis, i, remainingAxes)=>{
				val updatedCoords = coords.update(axis, i)
				val breadth = drawRules(o, b0, d0, depth, remainingAxes, updatedCoords)
				if (i != 0)
					o.choose(drawSeparator(gfx, axis, b0, d0, 0, depth),
							 drawSeparator(gfx, axis, d0, b0, depth, 0))
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
		
		def renderOwnLabels(b0: Int, d0: Int, availableDepth: Int,
						 axes: Seq[Axis], coords: Map[Axis, Int]): Int = {

			iterateAxis(b0, axes, coords, (b0, axis, i, remainingAxes)=>{
					val updatedCoords = coords.update(axis, i)
					val depth = labelDepth(o, axis, i)
					val breadth = renderOwnLabels(b0, d0, availableDepth - depth, remainingAxes, updatedCoords)
					var d1 = d0 - availableDepth
					val r = o.choose(new Rectangle(b0, d1, breadth, depth),
									 new Rectangle(d1, b0, depth, breadth))
					renderHeaderLabel(gfx, r, axis, i, updatedCoords)
					if (i != 0)
						o.choose(drawSeparator(gfx, axis, b0, d1, 0, availableDepth),
								 drawSeparator(gfx, axis, d1, b0, availableDepth, 0))
					breadth
				}, {
					renderChildLabels(gfx, o, b0, d0, availableDepth, coords)
				})
		}

		renderOwnLabels(o.breadth(dataOrigin), o.depth(dataOrigin), nearHeader(o), axes(o), coords)
	}


	def renderChildLabels(gfx: DrawingContext, o: Orientation, b0: Int, d0: Int, availableDepth: Int,
						  coords: Map[Axis, Int]): Int = {

		breadthOfCell(o, coords)
	}

	
	/*------------------------------------------------------------------------*/
	// HIT TESTING

	def hitTestCells(parent: Map[Axis, Int], p: Point): Selectable = {
		val hitX = hitTestAxis(Horizontal, p.x)
		val hitY = hitTestAxis(Vertical, p.y)
		(hitX, hitY) match {
			case (Some((cx, remainderX)), Some((cy, remainderY))) =>
				hitTestCell(parent++cx++cy, new Point(remainderX, remainderY))
			case _=> NullSelection
		}
	}


	def hitTestCell(coords: Map[Axis,Int], relative: Point): Selectable


	def hitTestLabels(parent: Map[Axis,Int], o: Orientation, b: Int, d0: Int): Selectable =
		hitTestAxis(o, b) match {
			case None => NullSelection
				
			case Some((axisCoords, deltaB)) =>
				def searchLabels(coords: Map[Axis,Int], remain: Seq[Axis], d: Int): Selectable =
					if (!remain.isEmpty) {
						val axis = remain.first
						val i = axisCoords(axis)
						val soFar = coords + (axis -> i)
						val thisD = labelDepth(o, axis, i)
						if (d < thisD)
							new OneLabel(this, o, soFar)
						else
							searchLabels(soFar,
										 remain.drop(1),
										 d - thisD)
						
					}else
						hitTestChildLabels(coords, o, deltaB, d0)

				searchLabels(parent, axes(o), d0 + nearHeader(o))
		}


	def hitTestChildLabels(parent: Map[Axis,Int], o: Orientation, b: Int, d: Int): Selectable


	def hitTestAxis(o: Orientation, b: Int): Option[(Map[Axis,Int], Int)]


	/*------------------------------------------------------------------------*/
	// NAVIGATION

	/**
	 * I don't know where my own labels are, but move 1 cell in one of the compass
	 * directions, and return NullSelection if we fall off the end of data grid or
	 * label area.
	 */
	def selectEdgeChild(context: Map[Axis,Int], plane: Orientation, end: End, hintSel: SingleGridSelection): Selectable
	def moveByOne(sel: SingleGridSelection, o: Orientation, d: Direction): Selectable

	/**
	 * Returns true iff this label is in my own label area. (This includes a struct’s
	 * child-blocks’ labels which have been promoted upwards.)
	 */
	def containsInEdgeArea(sel: OneLabel): Boolean

	def selectEdgeLabel(context: Map[Axis,Int], labels: Orientation, plane: Orientation, end: End, hintSel: SingleGridSelection): Selectable = {
		val theAxes = axes(labels)
		if (theAxes.isEmpty)
			NullSelection // No axes == no selection.
		else
			if (labels == plane){ // 'Short' (transverse) edge of label block.
				if (hintSel.coords.contains(theAxes(0))){
					val coords = context ++ theAxes.takeWhile(hintSel.coords.contains(_)).map(a => (a, end.of(a)))
					new OneLabel(this, labels, coords){
						override val actualB = 0 //TODO (hintSel.actualB)
						override val hintCoords = hintSel.coords ++ coords
					}
				} else{
					val a = theAxes(0)
					val coords = context + ((a, end.of(a)))
					new OneLabel(this, labels, coords){
						override val actualB = 0 /*TODO!*/
						override val hintCoords = hintSel.coords ++ coords
					}
				}

				
			}else{ // 'Long' (longitudinal) edge of label block
				val axes = if (end.isFirst) Seq(theAxes.first) else theAxes
				val coords = context ++ hintCoords(axes, hintSel.hintCoords)
				new OneLabel(this, labels, coords){
					override val actualB = 0/*TODO: actualB*/
					override val hintCoords = hintSel.coords ++ coords
				}
			}
	}

	protected def moveOwnLabelByOne(sel: OneLabel, o: Orientation, d: Direction): Selectable = {
		require(sel.block == this)
		val theAxes = axes(sel.orientation)
		if (o == sel.orientation) { // Moving longitudinally, along axes.
			val usedAxes = theAxes.takeWhile(sel.coords.contains(_))
			nextOnAxes(usedAxes, sel.coords, d) match {
				case Some(c) => new OneLabel(this, o, c)
				case None => NullSelection
			}
			
		}else{ // Moving transversely, across axes (between axes).
			val i = lastAxisIn(theAxes, sel.coords)
			val p = i + d.delta
			if (p >= 0 && p < theAxes.length) {
				val newCoords =
					if (d.isForward) {
						val ax = theAxes(p)
						val coord = sel.hintCoords.getOrElse(ax, 0)
						sel.coords + (ax -> coord)
					}else
						sel.coords - (theAxes(i))
					
				new OneLabel(this, sel.orientation, newCoords){
					override val actualB = sel.actualB
					override val hintCoords = sel.hintCoords ++ newCoords
				}
			}else
				NullSelection
		}
	}

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


	def labelBounds(dataOrigin: Point, lab: OneLabel): Rectangle = {
		if (lab.block == this) {
			val o = lab.orientation
			val longiRange = breadthOwnLabelBounds(o.breadth(dataOrigin), lab)
			val transRange = depthOwnLabelBounds(o.depth(dataOrigin), lab)
			o.newRectangle(longiRange.start, transRange.start, longiRange.length, transRange.length)

		}else{
			childLabelBounds(dataOrigin, lab)
		}
	}

	def childLabelBounds(dataOrigin: Point, lab: OneLabel): Rectangle

	def breadthOwnLabelBounds(offset: Int, lab: OneLabel): Range = {
		assert(lab.block == this)
		val o = lab.orientation; val coords = lab.coords
		val num = cellCountOf(o, coords)
		breadthOwnLabelBounds(offset, o, coords, num)
	}

	def breadthOwnLabelBounds(offset: Int, o: Orientation, coords: Map[Axis,Int], num: Int): Range


	def depthOwnLabelBounds(dataOrigin: Int, lab: OneLabel): Range = {
		assert(lab.block == this)
		val o = lab.orientation
		val coords = lab.coords
		val allDepths =	for(ax <- axes(o).take(lab.axisIndex + 1))
			yield labelDepth(o, ax, coords(ax))

		rangeOfLast(dataOrigin - nearHeader(o), allDepths)
	}


	/*------------------------------------------------------------------------*/
	// ITERATION

	def iterateValues(origin: Int, axes: Seq[Axis], coords: Map[Axis, Int],
					perValue: (Int, Map[Axis,Int])=>Int): Int = {

		iterateAxis(origin, axes, coords, (b0, axis, i, remainingAxes)=>{
				val updatedCoords = coords.update(axis, i)
				iterateValues(b0, remainingAxes, updatedCoords, perValue)
			}, {
				perValue(origin, coords)
			})
	}


	def iterateAxis(origin: Int, axes: Seq[Axis], coords: Map[Axis, Int],
				    perLabel: (Int, Axis, Int, Seq[Axis])=>Int,
					bottom: =>Int): Int = {

		if (!axes.isEmpty){
			val axis = axes(0)
			val remainingAxes = axes.drop(1)
			var offset = 0
			for(i <- axis.range){
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
				val max = axis.length
				val newP = p + delta
				if (newP < 0)
					// Before start of axis. Wrap around and move onto next axis:
					next(axes.drop(1), old + (axis -> axis.last))

				else if (newP < axis.length)
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
		++ (this.axes(o).map(a => (a -> anEnd.of(a))))
		++ (this match {
				case s: StructBlock if s.orientation == o =>
					s.elements(anEnd.of(s.structAxis)).axesEnd(o, anEnd)
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
			i = i * ax.length + c
		}
		i
	}

	def cellCountOf(o: Orientation, coords: Map[Axis, Int]): Int = {
		val axesWithValues = axes(o).dropWhile(coords.contains(_))
		(1 /: axesWithValues)((product, ax) => product * ax.length)
	}

	def arrayTable(coords: Map[Axis,Int]): ArrayTable


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

	
	/*------------------------------------------------------------------------*/
	// GFX UTILITY METHODS


	def renderHeaderLabel(gfx: DrawingContext, bounds: Rectangle, ax: Axis, index: Int, thisCoords: Map[Axis,Int]) {
		val selected = gfx.ui.selection match {
			case lab: OneLabel => (lab.block == this) && (thisCoords == lab.coords)
				
			case labs: LabelRange if (labs.block == this) && thisCoords.contains(labs.axis) =>
				val thisV = thisCoords(labs.axis)
				lazy val selCoords = labs.allCoordsButLast + ((labs.axis, thisV))
				(labs.range contains thisV) && (selCoords == thisCoords)

			case _=> false
		}
		renderBasicCell(gfx, labelStyle(ax, index), bounds,
						ax.label(index), selected)
	}


	def renderBasicCell(g: DrawingContext, style: CellStyle, bounds: Rectangle, value: String, selected: Boolean) {
		import g.gc
		val bg = if (selected) g.gc.getDevice().getSystemColor(SWT.COLOR_YELLOW)
						  else g.colorForRGB(style.backgroundColor)
		gc.setBackground(bg)
		gc.fillRectangle(bounds)
		gc.setForeground(g.colorForRGB(style.color))
		gc.setFont(g.font(style.fontFamily, style.fontSize, style.fontStyle))
		val fm = gc.getFontMetrics
		val h = fm.getAscent + fm.getDescent
		val y = (bounds.height - h) / 2
		val w = gc.stringExtent(value).x
		val x =
			if (style.textAlign == TextAlignLeft)
				style.marginLeft
			
			else if (style.textAlign == TextAlignCenter)
				(bounds.width - w) / 2

			else if (style.textAlign == TextAlignRight)
				bounds.width - style.marginRight - w

			else
				error("Unrecognised textAlign value "+style.textAlign)
			
		// Only set clipping if we need to:
		if (x < 0 || y < 0 || x+w >= bounds.width || y + h >= bounds.height)
			g.withclip(bounds){
				gc.drawString(value, bounds.x+x, bounds.y+y, true)
			}
		else
			gc.drawString(value, bounds.x+x, bounds.y+y, true)
	}

	
	def drawSeparator(gfx: DrawingContext, d: Axis, x0: Int, y0: Int, dx: Int, dy: Int){
		d.interItemLine match {
			case None => ;// No line
			case Some(l) => {
					l.setAttributesOf(gfx)
					gfx.gc.drawLine(x0, y0, x0+dx, y0+dy)
			}
		}
	}

}
