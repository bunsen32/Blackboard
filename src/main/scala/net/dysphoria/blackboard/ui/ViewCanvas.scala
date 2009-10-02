/*
 * App.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import org.eclipse.swt
import swt.SWT
import swt.events._
import swt.graphics._
import swt.widgets.{List=>_, _}
import blackboard.gfx._
import ui.Listeners._

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

	private val listener: Listener = handleEvent _
	Array(
		SWT.KeyDown, SWT.KeyUp,
		SWT.MouseDown, SWT.MouseUp, SWT.MouseWheel, SWT.MouseDoubleClick, SWT.DragDetect,
		SWT.MouseEnter, SWT.MouseExit, SWT.MouseMove,
		SWT.Help

	) foreach(evt => addListener(evt, listener))

	private def handleEvent(e: Event) {
		val state = new EventState(e.stateMask)
		val point = new Point(e.x, e.y)

		e.`type` match {
			case SWT.MouseDown =>
				val item = table.get.hitTest(Map.empty, point)
				ui.select(item)
				
			case _ => // ignore
		}
	}


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

