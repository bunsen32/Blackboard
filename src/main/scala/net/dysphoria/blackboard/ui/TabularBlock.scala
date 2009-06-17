/*
 * TabularBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._
import blackboard.data._
import DisplayDimension._
import selection._

/**
 * A DisplayBlock which particpates in the grid formed by the DimensionLists
 * which it intersects. As opposed to a block which is empty or just contains
 * a label or something.
 */
abstract class TabularBlock(val table: Table) extends DisplayBlock {
	def requireValidDimensionMap
	def dimensionMap: Map[DisplayDimension, Int]

	def uses(dd: DisplayDimension) = dimensionMap.contains(dd)

	def displayToTableCoordinates(xCoords: Seq[Int], yCoords: Seq[Int]): Seq[Int] = {
		val tableCoords = new Array[Int](table.arity)

		for((dispD, iv) <- (xDimensions.elements zip xCoords.elements))
			dimensionMap get dispD match {
				case Some(tableIndex) => tableCoords(tableIndex) = iv
				case None => ;//Ignore
			}

		for((dispD, iv) <- (yDimensions.elements zip yCoords.elements))
			dimensionMap get dispD match {
				case Some(tableIndex) => tableCoords(tableIndex) = iv
				case None => ;//Ignore
			}

		tableCoords
	}

	def tableToXDisplayCoordinates(coords: Seq[Int]) = tableToDisplayCoordinates(xDimensions, coords)
	def tableToYDisplayCoordinates(coords: Seq[Int]) = tableToDisplayCoordinates(yDimensions, coords)

	def tableToDisplayCoordinates(displayDims: List[DisplayDimension], coords: Seq[Int]) = {
		displayDims map (displayDim => dimensionMap.get(displayDim) match {
				case None => 0; // OR -1? This is a dimension which does not exist in the table.
				case Some(ix) => coords(ix)})
	}


	def render(gfx: DrawingContext, origin: Point){
		requireValidDimensionMap
		val sz = size
		renderRows(
			gfx,
			new Rectangle(origin.x, origin.y, sz.x, sz.y),
			xDimensions,
			yDimensions,
			new Array[Int](table.arity))
	}

	def renderRows(gfx: DrawingContext, bounds: Rectangle, dxs: List[DisplayDimension], dys: List[DisplayDimension], indices: Array[Int]) {
		if (dys.exists(dd => dimensionMap contains dd)) {
			val (dy :: others) = dys
			val dimensionIx = dimensionMap get dy
			val childBounds = new Rectangle(bounds.x, bounds.y, bounds.width, 0)
			var isSubsequent = false
			for(i <- dy.dim.domain){
				for (d <- dimensionIx) indices(d) = i

				childBounds.height = itemWidthOf(dys, indices)
				renderRows(gfx, childBounds, dxs, others, indices)
				if (isSubsequent)
					drawSeparator(gfx, dy, childBounds.x, childBounds.y, childBounds.x+childBounds.width, childBounds.y)

				childBounds.y += childBounds.height
				isSubsequent = true
			}

		}else
			renderRowCells(gfx, bounds, dxs, indices)
	}

	def renderRowCells(gfx: DrawingContext, bounds: Rectangle, dxs: List[DisplayDimension], indices: Array[Int]) {
		if (dxs.exists(dd => dimensionMap contains dd)) {
			val (dx :: others) = dxs
			val dimensionIx = dimensionMap get dx
			val childBounds = new Rectangle(bounds.x, bounds.y, 0, bounds.height)
			var isSubsequent = false
			for(i <- dx.dim.domain){
				for (d <- dimensionIx) indices(d) = i

				childBounds.width = itemWidthOf(dxs, indices)
				renderRowCells(gfx, childBounds, others, indices)
				if (isSubsequent)
					drawSeparator(gfx, dx, childBounds.x, childBounds.y, childBounds.x, childBounds.y + childBounds.height)

				childBounds.x += childBounds.width
				isSubsequent = true
			}

		}else
			renderCell(gfx, bounds, indices, defaultCellStyle)
	}

	def drawSeparator(gfx: DrawingContext, d: DisplayDimension, x0: Int, y0: Int, x1: Int, y1: Int){
		d.interItemLine match {
			case None => ;// No line
			case Some(l) => {
					l.setAttributesOf(gfx)
					gfx.gc.drawLine(x0, y0, x1, y1)
			}
		}
	}

	val black = new RGB(0, 0, 0)
	val white = new RGB(255, 255, 255)
	val yellow = new RGB(255, 255, 0)
	val lightYellow = new RGB(255, 255, 200)

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Array[Int], cellStyle: CellStyle) {
		val value = table.valueToString(table.applyByIndex(indices))

		import gfx._
		withclip(bounds){
			val selectFactor = cellIsSelected(gfx.ui, indices) match {case 0 => 0F;case 1=>0.33333F; case _ => 1F}
			val bg = Style.mix(cellStyle.backgroundColor, selectFactor, yellow)
			gc.setBackground(gfx.colorForRGB(bg))
			gc.fillRectangle(bounds)
			gc.setForeground(gfx.colorForRGB(cellStyle.color))
			gc.setFont(gfx.font(cellStyle.fontFamily, cellStyle.fontSize, cellStyle.fontStyle))
			val fm = gc.getFontMetrics
			val h = fm.getAscent + fm.getDescent
			val y = (bounds.height - h)/2
			val x = 
				if (cellStyle.textAlign == TextAlignLeft)
					cellStyle.marginLeft
				else{
					val w = gc.stringExtent(value).x
					if (cellStyle.textAlign == TextAlignCenter)
						(bounds.width - w)/2

					else if (cellStyle.textAlign == TextAlignRight)
						bounds.width - cellStyle.marginRight - w

					else
						error("Unrecognised textAlign value "+cellStyle.textAlign)
				}
			gc.drawString(value, bounds.x+x, bounds.y+y, true)
		}
	}


	def cellIsSelected(ui: UIState, indices: Array[Int]) = ui.selection match {
		case s: HasDisplayDimensionValues => {
			val dValues = s.dimensionValues
			var containsAnyDimensions = false
			var matchesSomeDimensionVals = false
			var matchesAllDimensionVals = true
			dValues.foreach(kv => {
				val (dd, vals) = kv
				if (!vals.isEmpty){
					dimensionMap.get(dd) match {
						case None => ;// ignore
						case Some(ix) => {
								containsAnyDimensions = true
								val matches = vals.contains(indices(ix))
								matchesSomeDimensionVals |= matches
								matchesAllDimensionVals &= matches
						}
					}
				}
			})
			if (containsAnyDimensions && matchesAllDimensionVals)
				2
			else if (matchesSomeDimensionVals)
				1
			else
				0
		}
		case _ => 0
	}

	val defaultCellStyle = new CellStyle

	{
		import defaultCellStyle._
		marginLeft = 2
		marginRight= 2
	}
}
