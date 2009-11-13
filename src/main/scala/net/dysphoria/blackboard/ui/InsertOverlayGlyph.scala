/*
 * InsertOverlay.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import gfx.pathToRegion

abstract class InsertOverlayGlyph(d: Device) extends NodeGlyph(d) {
	private lazy val origin = new Point(15, 15)
	val rotation: Int
	val alt: Boolean
	private lazy val sin = Math.sin(Math.toRadians(rotation))
	private lazy val cos = Math.cos(Math.toRadians(rotation))

	def setupRegion(r: Region) = {
		val p = new Path(r.getDevice)
		try {
			p.addArc(origin.x - 9, origin.y - 9, 18, 18, 0, 360)
			val d = 7.071067811865F
			val (x0, y0) = (d, -d)
			val (x1, y1) = (d*2, 0)
			val (x2, y2) = (d, d)
			p.moveTo(transX(x0, y0), transY(x0, y0))
			p.lineTo(transX(x1, y1), transY(x1, y1))
			p.lineTo(transX(x2, y2), transY(x2, y2))
			p.close
			pathToRegion(r, p)
			r
			
		} finally
			p.dispose
	}

	def transX(x0: Float, y0: Float) =
		(origin.x + (cos * x0) - (sin * y0)).toFloat

	def transY(x0: Float, y0: Float) =
		(origin.y - (sin * x0) - (cos * y0)).toFloat

	
    def onPaint(gc: GC, armed: Boolean, focused: Boolean) {
		val device = gc.getDevice
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)

		val fgColour = device.getSystemColor(
			if (alt) SWT.COLOR_WHITE else SWT.COLOR_BLACK)

		val bgColour = device.getSystemColor(if (armed)
				if (alt) SWT.COLOR_DARK_YELLOW else SWT.COLOR_YELLOW
			else if (focused)
				if (alt) SWT.COLOR_DARK_GRAY else SWT.COLOR_WHITE
			else
				if (alt) SWT.COLOR_BLACK else SWT.COLOR_GRAY)

		gc.setBackground(fgColour)
		gc.fillRectangle(0, 0, 100, 100)
		gc.setBackground(bgColour)
		gc.fillOval(origin.x - 8, origin.y - 8, 16, 16)
		gc.setBackground(fgColour)
		gc.fillRectangle(origin.x - 1, origin.y - 6, 2, 12)
		gc.fillRectangle(origin.x - 6, origin.y - 1, 12, 2)
	}
}
