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

abstract class ViewCanvas(parent: Composite, style: Int) extends Composite(parent, SWT.H_SCROLL|SWT.V_SCROLL) {
    private val Origin = new Point(0, 0)
    val white = new RGB(255, 255, 255)
	val black = new RGB(0, 0, 0)
	val red = new RGB(255, 0, 0)

	private val listener: Listener = handleEvent _
	private val cellEditListener: Listener = handleCellEditEvent _
	private val interestingEvents = Array(
		SWT.Resize,
		SWT.KeyDown, SWT.KeyUp,
		SWT.MouseDown, SWT.MouseUp, SWT.MouseWheel, SWT.MouseDoubleClick, SWT.DragDetect,
		SWT.MouseEnter, SWT.MouseExit, SWT.MouseMove,
		SWT.Help
	)

	
	val table: Table
	val navigator = new Navigator {
		val table = ViewCanvas.this.table
	}
	val ui = new UIState(this)
	val cellEdit = new CellEditor(this)
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

	type GeometryChangedListener = Function[ViewCanvas,Unit]
	val geometryChangedListeners = new mutable.HashSet[GeometryChangedListener]
	

	addDisposeListener((e: DisposeEvent) => {
			// Not much to do
						})
    addPaintListener(paintControl _)

	interestingEvents foreach(evt => addListener(evt, listener))
	getVerticalBar.addListener(SWT.Selection, scrollVertical _)
	getHorizontalBar.addListener(SWT.Selection, scrollHorizontal _)

	cellEdit.addListener(SWT.MouseWheel, listener)
	cellEdit.addListener(SWT.KeyDown, cellEditListener)
	cellEdit.addListener(SWT.Traverse, cellEditListener)

	private def handleCellEditEvent(e: Event) {
		e.`type` match {
			case SWT.Traverse => e.doit = false
			case SWT.KeyDown => if (processCellEditKey(e)) e.doit = false
			case _ => false
		}
	}

	private def handleEvent(e: Event) {
		val state = new EventState(e.stateMask)
		val point = viewToModel(new Point(e.x, e.y))

		e.`type` match {
			// Events which fire regardless of widget:
			case SWT.MouseWheel=> if (state.isAltBehaviour) {
				val newScale = (0.2F max power(scale, 1.05F, e.count) min 5.0F)
				if (scale != newScale) {
					// 1:1 scale is special. If the zoom 'crosses' 1:1, make it
					// exactly 1:1
					val actualNewScale = if (scale < 1F && newScale >1F || scale > 1F && newScale < 1F) 1F else newScale
					scale = actualNewScale
					val newScaledPoint = viewToModel(new Point(e.x, e.y))
					idealOffsetX = offsetX + point.x - newScaledPoint.x
					idealOffsetY = offsetY + point.y - newScaledPoint.y
					computeBounds
				}
				e.doit = false // consume the event
			}
			case _ if e.widget != this => ; // skip anything else

			// Events specific to the ViewCanvas widget.
			case SWT.Resize => computeBounds

			case SWT.MouseDown =>
				val item = selectableThingAt(point)
				if (item != ui.selection)
					ui.select(item)
				else
					ui.fineEditMode = true
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

	private def scrollHorizontal(e: Event){
		val unscaled = getHorizontalBar.getSelection
		val x = (unscaled / 256F) + minX
		if (x != offsetX)
			setOrigin(x, offsetY)
	}
	private def scrollVertical(e: Event){
		val unscaled = getVerticalBar.getSelection
		val y = (unscaled / 256F) + minY
		if (y != offsetY)
			setOrigin(offsetX, y)
	}

	private def power(result: Float, d: Float, e: Int): Float =
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
	 * If the model (table) size, or zoom factor, or view (control) size, has changed,
	 * we need to recompute the view area available and scroll position. Et voila.
	 * Provides the limits for scrolling, and updates the scrollbars.
	 * Do not confuse with (SWT-compatible) method 'computeSize'.
	 */
	private def computeBounds {
		val clientArea = getClientArea
		val canvasWidth = (clientArea.width / scale)
		val canvasHeight = (clientArea.height / scale)
		val model = table.size

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

		updateOrigin(true)
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

	/*def scrollTo(x: Float, y: Float) {
		setOrigin(x, y)
		val xScroll = getHorizontalBar
		val yScroll = getVerticalBar
		xScroll.setSelection(((offsetX - minX) * 256F).toInt)
		yScroll.setSelection(((offsetY - minY) * 256F).toInt)
	}*/

	private def setOrigin(x: Float, y: Float){
		idealOffsetX = x
		idealOffsetY = y
		updateOrigin(false)
	}

	/**
	 * The idealOffset*s (scroll position) have changed, or else the world has
	 * changed about them. In any case, recompute the ACTUAL offset*s and trigger a
	 * redraw if they’ve changed.
	 */
	private def updateOrigin(scaleChanged: Boolean) {
		def updateListeners =
			for(l <- geometryChangedListeners)
				l.apply(this)

		val x = minX.toFloat max idealOffsetX min maxX
		val y = minY.toFloat max idealOffsetY min maxY
		if (x != offsetX || y != offsetY) {
			offsetX = x; offsetY = y
			redraw
			updateListeners
		}else
			if (scaleChanged) updateListeners
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

	private def d_+ = true
	private def d_- = false

	def processKey(e: Event){
		val shift = (e.stateMask & SWT.SHIFT) != 0
		e.keyCode match {
			case SWT.ARROW_UP => moveSelection(YOrientation, d_-, ByOne)
			case SWT.ARROW_DOWN => moveSelection(YOrientation, d_+, ByOne)
			case SWT.ARROW_LEFT => moveSelection(XOrientation, d_-, ByOne)
			case SWT.ARROW_RIGHT => moveSelection(XOrientation, d_+, ByOne)
			case SWT.TAB if shift => moveSelection(XOrientation, d_-, ByOne)
			case SWT.TAB if !shift => moveSelection(XOrientation, d_+, ByOne)
			case _ => //ignore
		}
	}

	def processCellEditKey(e: Event): Boolean = {
		val shift = (e.stateMask & SWT.SHIFT) != 0
		val input = cellEdit.input
		val selection = input.getSelection
		val selStart = selection.x
		val emptySelection = (selection.y == selStart)
		val atLeft = emptySelection && (selStart == cellEdit.leftPosition)
		val atRight = emptySelection && (selStart == cellEdit.rightPosition)
		e.keyCode match {
			case SWT.ARROW_UP => moveSelection(YOrientation, d_-, ByOne); true
			case SWT.ARROW_DOWN => moveSelection(YOrientation, d_+, ByOne); true
			case SWT.ARROW_LEFT if atLeft => moveSelection(XOrientation, d_-, ByOne); true
			case SWT.ARROW_RIGHT if atRight => moveSelection(XOrientation, d_+, ByOne); true
			case SWT.TAB if shift => moveSelection(XOrientation, d_-, ByOne); true
			case SWT.TAB if !shift => moveSelection(XOrientation, d_+, ByOne); true

			case SWT.ESC => ui.fineEditMode = false; true
			case _ => false
		}
	}

	def moveSelection(o: Orientation, incNotDec: Boolean, granularity: MovementGranularity){
		ui.selection match {
			case cell: CellSelection =>
				granularity match {
					case ByOne => ui.select(navigator.moveByCell(cell, o, incNotDec).orElse(cell))
					case _ => //ignore
				}
			case lab: LabelSelection =>
				granularity match {
					case ByOne => ui.select(navigator.moveByOne(lab, o, incNotDec).orElse(lab))
					case _=>
				}
			case _ => //ignore
		}
	}


	/**
	 * Deliver the preferred size of the control. Assumes that 'table' already
	 * has an available computed size.
	 */
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

