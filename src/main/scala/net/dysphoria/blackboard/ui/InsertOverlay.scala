/*
 * InsertOverlay.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.events._
import org.eclipse.swt.graphics._
import org.eclipse.swt.widgets._
import Listeners._
import gfx.pathToRegion

abstract class InsertOverlay(parent: Shell) extends Overlay(parent) {
	private val origin = new Point(12, 12)
	val rotation: Double
	private val sin = Math.sin(rotation)
	private val cos = Math.cos(rotation)

	val listener: Listener = handleEvent _
	Seq(
		SWT.Dispose, SWT.Paint,
		SWT.MouseDown, SWT.MouseUp, SWT.DragDetect,
		SWT.MouseEnter, SWT.MouseExit, SWT.MouseMove,
		SWT.Help)
		.foreach(e => shell.addListener(e, listener))

	shell.setSize(100, 100)
	private val _region = createRegion
	shell.setRegion(_region)

	def createRegion = {
		val p = new Path(shell.getDisplay)
		try {
			p.addArc(origin.x - 11, origin.y - 11, 22, 22, 0, 360)
			val d = 7.071067811865F
			val (x0, y0) = (d, -d)
			val (x1, y1) = (d*2, 0)
			val (x2, y2) = (d, d)
			p.moveTo(transX(x0, y0), transY(x0, y0))
			p.lineTo(transX(x1, y1), transY(x1, y1))
			p.lineTo(transX(x2, y2), transY(x2, y2))
			p.close
			val r = new Region(shell.getDisplay)
			pathToRegion(r, p)
			r
			
		} finally
			p.dispose
	}

	def handleEvent(e: Event) {
		e.`type` match {
			case SWT.Dispose => onDispose(e)
			case SWT.Paint => onPaint(e)
			case _ => // Ignore anything else.
		}
	}

	def onDispose(e: Event) {
		if (_region != null) _region.dispose
	}

    def onPaint(e: Event) {
		val gc = e.gc
		val device = e.display
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)

		val black = device.getSystemColor(SWT.COLOR_BLACK)
		val white = device.getSystemColor(SWT.COLOR_WHITE)
		gc.setBackground(black)
		gc.fillRectangle(0, 0, 100, 100)
		gc.setBackground(white)
		gc.fillOval(origin.x - 10, origin.y - 10, 20, 20)
		gc.setBackground(black)
		gc.fillRectangle(origin.x - 1, origin.y - 6, 2, 12)
		gc.fillRectangle(origin.x - 6, origin.y - 1, 12, 2)
	}

	def transX(x0: Float, y0: Float) =
		(origin.x + (cos * x0) - (sin * y0)).toFloat

	def transY(x0: Float, y0: Float) =
		(origin.x + (sin * x0) + (cos * y0)).toFloat
}
