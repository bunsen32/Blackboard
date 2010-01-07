/*
 * InsertOverlay.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import net.dysphoria.blackboard._
import gfx.pathToRegion

class DeleteOverlayGlyph(d: Device) extends NodeGlyph(d) {
	private lazy val origin = new Point(15, 15)

	def setupRegion(r: Region) = {
		val p = new Path(r.getDevice)
		try {
			p.addArc(origin.x - 9, origin.y - 9, 18, 18, 0, 360)
			pathToRegion(r, p)
			r

		} finally
			p.dispose
	}

    def onPaint(gc: GC, armed: Boolean, focused: Boolean) {
		val device = gc.getDevice
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)

		val dark = device.getSystemColor(SWT.COLOR_BLACK)

		val light = device.getSystemColor(if (armed)
				SWT.COLOR_YELLOW
			else if (focused)
				SWT.COLOR_WHITE
			else
				SWT.COLOR_GRAY)

		val (fg, bg) = (light, dark)

		gc.setBackground(fg)
		gc.fillRectangle(0, 0, 100, 100)
		gc.setBackground(bg)
		gc.fillOval(origin.x - 8, origin.y - 8, 16, 16)
		gc.setForeground(fg)
		gc.setLineWidth(2)
		gc.drawLine(origin.x - 5, origin.y - 5, origin.x + 5, origin.y + 5)
		gc.drawLine(origin.x - 5, origin.y + 5, origin.x + 5, origin.y - 5)
	}
}
