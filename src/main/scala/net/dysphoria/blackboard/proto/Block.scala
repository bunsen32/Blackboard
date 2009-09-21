/*
 * Block.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import ui.{Displayable, Orientation, XOrientation, YOrientation}
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import blackboard.gfx._

abstract class Block extends Displayable {
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

	def preferredLabelDepth(o: Orientation, a: Axis, i: Int) =
		if (a.label(i) == "")
			0
		else
			if (o.isX) xLabelHeight(a, i) else yLabelWidth(a, i)

	def labelDepth(o: Orientation, a: Axis, i: Int) =
		if (o.isX) xLabelHeight(a, i) else yLabelWidth(a, i)

	def xLabelHeight(a: Axis, i: Int) = genericCellHeight
	def yLabelWidth(a: Axis, i: Int) = genericCellWidth

	def axes(o:Orientation) = if (o.isX) xAxes else yAxes
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

	def render(g: DrawingContext, origin: Point) = {
		val dataOrigin = new Point(origin.x + leftHeader, origin.y + topHeader)

		renderCells(g, dataOrigin, Map.empty)
		//Headers
		renderLabels(g, dataOrigin, XOrientation, Map.empty)
		renderLabels(g, dataOrigin, YOrientation, Map.empty)
	}

	def size = outerSize

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]): Int

	def renderHeaderLabel(gfx: DrawingContext, bounds: Rectangle, ax: Axis, index: Int) {
		renderBasicCell(gfx, labelStyle(ax, index), bounds,
						ax.label(index))
	}

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int])

	def renderBasicCell(g: DrawingContext, style: CellStyle, bounds: Rectangle, value: String) {
		g.withclip(bounds){
			import g.gc
			gc.setBackground(g.colorForRGB(style.backgroundColor))
			gc.fillRectangle(bounds)
			gc.setForeground(g.colorForRGB(style.color))
			gc.setFont(g.font(style.fontFamily, style.fontSize, style.fontStyle))
			val fm = gc.getFontMetrics
			val h = fm.getAscent + fm.getDescent
			val y = (bounds.height - h)/2
			val x =
				if (style.textAlign == TextAlignLeft)
					style.marginLeft
				else{
					val w = gc.stringExtent(value).x
					if (style.textAlign == TextAlignCenter)
						(bounds.width - w)/2

					else if (style.textAlign == TextAlignRight)
						bounds.width - style.marginRight - w

					else
						error("Unrecognised textAlign value "+style.textAlign)
				}
			gc.drawString(value, bounds.x+x, bounds.y+y, true)
		}
	}

	
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
			val h = breadthOfCell(YOrientation, rowCoords)
			iterateValues(origin.x, xAxes, rowCoords, (x, cellCoords)=>{
				val w = breadthOfCell(XOrientation, cellCoords)
				val bounds = new Rectangle(x, y, w, h)
				renderCell(gfx, bounds, cellCoords)
				w
			})
			h
		})

		drawRules(XOrientation, origin.x, origin.y, innerSize.y, xAxes, coords)
		drawRules(YOrientation, origin.y, origin.x, innerSize.x, yAxes, coords)
	}


	def renderLabels(gfx: DrawingContext, dataOrigin: Point, o:Orientation, coords: Map[Axis, Int]){
		
		def renderOwnLabels(gfx: DrawingContext, o: Orientation, b0: Int, d0: Int, availableDepth: Int,
						 axes: Seq[Axis], coords: Map[Axis, Int]): Int = {

			iterateAxis(b0, axes, coords, (b0, axis, i, remainingAxes)=>{
					val updatedCoords = coords.update(axis, i)
					val depth = labelDepth(o, axis, i)
					val breadth = renderOwnLabels(gfx, o, b0, d0, availableDepth - depth, remainingAxes, updatedCoords)
					var d1 = d0 - availableDepth
					val r = o.choose(new Rectangle(b0, d1, breadth, depth),
									 new Rectangle(d1, b0, depth, breadth))
					renderHeaderLabel(gfx, r, axis, i)
					if (i != 0)
						o.choose(drawSeparator(gfx, axis, b0, d1, 0, availableDepth),
								 drawSeparator(gfx, axis, d1, b0, availableDepth, 0))
					breadth
				}, {
					renderChildLabels(gfx, o, b0, d0, availableDepth, coords)
				})
		}

		if (o.isX)
			renderOwnLabels(gfx, o, dataOrigin.x, dataOrigin.y, topHeader, xAxes, coords)
		else
			renderOwnLabels(gfx, o, dataOrigin.y, dataOrigin.x, leftHeader, yAxes, coords)
	}


	def renderChildLabels(gfx: DrawingContext, o: Orientation, b0: Int, d0: Int, availableDepth: Int,
						  coords: Map[Axis, Int]): Int = {

		breadthOfCell(o, coords)
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

	def iterateValues(origin: Int, axes: Seq[Axis], coords: Map[Axis, Int],
					perValue: (Int, Map[Axis,Int])=>Int): Int = {
		/* Should be equivalent to: */
		iterateAxis(origin, axes, coords, (b0, axis, i, remainingAxes)=>{
				val updatedCoords = coords.update(axis, i)
				iterateValues(b0, remainingAxes, updatedCoords, perValue)
			}, {
				perValue(origin, coords)
			})
		/*
		if (!axes.isEmpty) {
			val a = axes(0)
			val remainingAxes = axes.drop(1)
			var offset = 0
			for(i <- a.range){
				val updatedCoords = coords.update(a, i)
				val d = iterateValues(origin + offset, remainingAxes, updatedCoords, perValue)
				offset += d
			}
			offset

		} else {
			perValue(origin, coords)
		}*/
	}

	def iterateAxis(origin: Int, axes: Seq[Axis], coords: Map[Axis, Int],
				  perLabel: (Int, Axis, Int, Seq[Axis])=>Int, bottom: =>Int): Int = {

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

}
