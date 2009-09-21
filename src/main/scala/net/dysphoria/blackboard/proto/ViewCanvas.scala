/*
 * App.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import scala.collection.mutable
import org.eclipse.swt
import swt.SWT
import swt.events._
import swt.graphics._
import swt.widgets.{List=>_, _}
import blackboard.gfx._
import ui.Listeners._
import ui.UIState

class ViewCanvas(parent: Composite, style: Int) extends Canvas(parent, SWT.H_SCROLL|SWT.V_SCROLL) {
    private val Origin = new Point(0, 0)
    val white = new RGB(255, 255, 255)
	val black = new RGB(0, 0, 0)
	val red = new RGB(255, 0, 0)

	var table: Option[Block] = None
	val ui = new UIState(this)
	var mouseX = -1
	var mouseY = 0
	var scale = 1.0F

	var idealOffsetX = 0F
	var idealOffsetY = 0F
	var offsetX = 0F
	var offsetY = 0F
	var minX = 0F
	var minY = 0F
	var maxX = 0F
	var maxY = 0F

	addDisposeListener((e: DisposeEvent) => {
			// Not much to do
						})
    addPaintListener(paintControl _)

    def paintControl(e: PaintEvent) {
		val gc = e.gc
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)
		val gfx = new DrawingContext(gc, ui)
		try{
			val canvasArea = getClientArea

			gc.setBackground(gfx.colorForRGB(white))
			gc.fillRectangle(0, 0, canvasArea.x, canvasArea.y)

			table match {
				case Some(t) => t.render(gfx, new Point(0,0))
				case None => ;//ignore
			}
			if (mouseX != -1){
				gc.setForeground(gfx.colorForRGB(red))
				gc.drawLine(mouseX, 0, mouseX, canvasArea.height)
				gc.drawLine(0, mouseY, canvasArea.width, mouseY)
			}

		}catch{
			case ex => {println(ex); throw ex}

		}finally{
			gfx.dispose
		}
    }

    override def computeSize(wHint: Int, hHint: Int, changed: Boolean) = new Point(1000, 1000)

}
