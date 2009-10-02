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
import ui.selection._

abstract class ViewCanvas(parent: Composite, style: Int) extends Canvas(parent, SWT.H_SCROLL|SWT.V_SCROLL) {
    private val Origin = new Point(0, 0)
    val white = new RGB(255, 255, 255)
	val black = new RGB(0, 0, 0)
	val red = new RGB(255, 0, 0)

	val table: Block
	val navigator = new Navigator {
		val topTable = table
	}
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
		SWT.Resize,
		SWT.KeyDown, SWT.KeyUp,
		SWT.MouseDown, SWT.MouseUp, SWT.MouseWheel, SWT.MouseDoubleClick, SWT.DragDetect,
		SWT.MouseEnter, SWT.MouseExit, SWT.MouseMove,
		SWT.Help

	) foreach(evt => addListener(evt, listener))
	getVerticalBar.addListener(SWT.Selection, scrollVertical _)
	getHorizontalBar.addListener(SWT.Selection, scrollHorizontal _)

	private def handleEvent(e: Event) {
		val state = new EventState(e.stateMask)
		val point = viewToModel(new Point(e.x, e.y))

		e.`type` match {
			case SWT.Resize => computeBounds
			case SWT.MouseDown =>
				val item = selectableThingAt(point)
				ui.select(item)
				/*if (state.isExtendSelect)
					ui.extendSelectionTo(hitThing)

				else if (state.isMultiSelect)
					ui.toggle(hitThing)

				else {
					if (ui.selection contains hitThing){ // potential drag
						ui.focus = hitThing
						ui.dragState = MouseDown(remainder.x, remainder.y) // Doesn't /really/ start til mouse moves.

					} else {
						ui.select(hitThing)
					}
				}*/
				
			case SWT.MouseUp => {
				if (ui.dragState == MouseDown)
					ui.select(ui.focus)
				ui.dragState = NoDrag
			}
			case SWT.MouseMove => {
				ui.dragState match {
					case NoDrag => {
						val isDragSelect = state.isPrimaryButton
						if (isDragSelect) {
							val hitThing = selectableThingAt(point)
							ui.extendSelectionTo(hitThing)
						}
						//mouseX = e.x; mouseY = e.y
					}
					case _ => ;//ignore
				}
			}
			case SWT.DragDetect => {
				if (ui.dragState.isInstanceOf[MouseDown]){
					ui.selection match {
						//case b: SingleGridSpace => DragOperation.start(new BlockDragClient(this))
						case _ => ;//ignore
					}
				}
			}
			case SWT.MouseExit => {mouseX = -1; redraw}
			case SWT.MouseEnter=> ;
			case SWT.MouseWheel=> if (state.isAltBehaviour) {
				val newScale = (0.2F max power(scale, 1.05F, e.count) min 5.0F)
				if (scale != newScale) {
					scale = newScale
					val newScaledPoint = viewToModel(new Point(e.x, e.y))
					idealOffsetX = offsetX + point.x - newScaledPoint.x
					idealOffsetY = offsetY + point.y - newScaledPoint.y
					computeBounds
				}
				e.doit = false // consume the event
			}
			case SWT.KeyDown => {
				if (e.keyCode == SWT.MOD3) ui.selectLargeBits = true
				processKey(e)
			}
			case SWT.KeyUp => {
				if (e.keyCode == SWT.MOD3) ui.selectLargeBits = false
			}
			case _ => /* ignore */;
		}
	}

	def scrollHorizontal(e: Event){
		val unscaled = getHorizontalBar.getSelection
		val x = (unscaled / 256F) + minX
		if (x != offsetX)
			setOrigin(x, offsetY)
	}
	def scrollVertical(e: Event){
		val unscaled = getVerticalBar.getSelection
		val y = (unscaled / 256F) + minY
		if (y != offsetY)
			setOrigin(offsetX, y)
	}

	def power(result: Float, d: Float, e: Int): Float =
		if (e == 0)
			result
		else if (e < 0)
			power(result/d, d, e+1)
		else
			power(result*d, d, e-1)



	def selectableThingAt(point: Point): Selectable = {
		table.hitTest(Map.empty, point)
	}
	//def selectableThingAndRemainderAt(absolute: Point): (Selectable, Point) = {
		//throw new NotImplementedException
	//}


	/**
	 * If the physical size, or zoom factor, or window size, has changed,
	 * we need to recompute the view area available and scroll position. Et voila:
	 */
	def computeBounds {
		val clientArea = getClientArea
		val canvasWidth = (clientArea.width / scale)
		val canvasHeight = (clientArea.height / scale)
		val model = table.outerSize

		if (model.x > canvasWidth){
			minX = 0
			maxX = model.x - canvasWidth
		}else{
			minX = (model.x - canvasWidth) / 2
			maxX = minX
		}

		if (model.y > canvasHeight){
			minY = 0
			maxY = model.y - canvasHeight
		}else{
			minY = (model.y - canvasHeight) / 2
			maxY = minY
		}

		updateOrigin
		val xScroll = getHorizontalBar
		val xPage = (canvasWidth * 256).toInt
		val xSize = ((canvasWidth max model.x) * 256).toInt
		val xSmall = 20*256
		xScroll.setValues(
			((offsetX - minX) * 256F).toInt, // selection
			0, // minimum
			xSize, // maximum
			xPage, // thumb
			xSmall, // increment
			(xPage - xSmall) max xSmall) // pageIncrement

		val yScroll = getVerticalBar
		val yPage = (canvasHeight * 256).toInt
		val ySize = ((canvasHeight max model.y) * 256).toInt
		val ySmall = 20*256
		yScroll.setValues(
			((offsetY - minY) * 256F).toInt, // selection
			0, // minimum
			ySize, // maximum
			yPage, // thumb
			ySmall, // increment
			(yPage - ySmall) max ySmall) // pageIncrement
	}

	def scrollTo(x: Float, y: Float) {
		setOrigin(x, y)
		val xScroll = getHorizontalBar
		val yScroll = getVerticalBar
		xScroll.setSelection(((offsetX - minX) * 256F).toInt)
		yScroll.setSelection(((offsetY - minY) * 256F).toInt)
	}

	private def setOrigin(x: Float, y: Float){
		idealOffsetX = x
		idealOffsetY = y
		updateOrigin
	}

	private def updateOrigin {
		val x = minX.toFloat max idealOffsetX min maxX
		val y = minY.toFloat max idealOffsetY min maxY
		if (x != offsetX || y != offsetY) {
			offsetX = x; offsetY = y
			redraw
		}
	}

	def viewToModel(p: Point): Point = viewToModel(p.x, p.y)
	def viewToModel(x: Int, y: Int) = new Point(
		(x / scale + offsetX).toInt,
		(y / scale + offsetY).toInt)

	def modelToView(p: Point): Point = modelToView(p.x, p.y)
	def modelToView(x: Int, y: Int) = new Point(
		((x - offsetX) * scale).toInt,
		((y - offsetY) * scale).toInt)

    def paintControl(e: PaintEvent) {
		val gc = e.gc
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)
		val gfx = new DrawingContext(gc, ui)
		try{
			val canvasArea = getClientArea
			val gridSize = table.size

			gfx.withclip(new Rectangle(0, 0, canvasArea.width, canvasArea.height)){
				val trans = gfx.newTransform
				trans.identity
				trans.scale(scale.toFloat, scale.toFloat)
				trans.translate(- offsetX.toFloat, - offsetY.toFloat)
				gc.setTransform(trans)

				gc.setBackground(gfx.colorForRGB(white))
				gc.fillRectangle(0, 0, gridSize.x, gridSize.y)

				table.render(gfx, Origin)

				trans.identity
				gc.setTransform(trans)
				for(d <- ui.dropTarget)
					d.render(gfx, new Point(0, 0))
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


	def processKey(e: Event){
		e.keyCode match {
			case SWT.ARROW_UP => moveSelection(YOrientation, -1, ByOne)
			case SWT.ARROW_DOWN => moveSelection(YOrientation, +1, ByOne)
			case SWT.ARROW_LEFT => moveSelection(XOrientation, -1, ByOne)
			case SWT.ARROW_RIGHT => moveSelection(XOrientation, +1, ByOne)
			case _ => println("unrecognised key")
		}
	}

	def moveSelection(o: Orientation, delta: Int, granularity: MovementGranularity){
		ui.selection match {
			case cell: CellSelection =>
				granularity match {
					case ByOne => ui.select(navigator.moveByCell(cell, o, delta).orElse(cell))
					case _ => //ignore
				}
			case _ => //ignore
		}
	}


    override def computeSize(wHint: Int, hHint: Int, changed: Boolean) = {
		try{
			val clientSize = table.size
			val idealSize = computeTrim(0, 0, clientSize.x, clientSize.y)
			new Point(
				(if (wHint != SWT.DEFAULT) wHint else idealSize.width),
				(if (hHint != SWT.DEFAULT) hHint else idealSize.height))

		}catch{
			case ex => {println(ex); throw ex}
		}
    }

}

