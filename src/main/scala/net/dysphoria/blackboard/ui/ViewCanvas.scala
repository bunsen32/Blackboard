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
import swt.widgets.{Composite, Event, Listener, Menu}

import net.dysphoria.blackboard._
import gfx._
import ui.Listeners._
import ui.selection._
import ui.actions._
import ui.model._

abstract class ViewCanvas(parent: Composite, style: Int) extends Composite(parent, SWT.H_SCROLL|SWT.V_SCROLL|SWT.DOUBLE_BUFFERED) {
	private val Origin = new Point(0, 0)
	val white = new RGB(255, 255, 255)
	val black = new RGB(0, 0, 0)
	val red = new RGB(255, 0, 0)

	private val listener: Listener = handleEvent _
	private val cellEditListener: Listener = handleCellEditEvent _
	private val interestingEvents = Array(
		SWT.Resize,
		SWT.KeyDown, SWT.KeyUp, SWT.FocusOut,
		SWT.MouseDown, SWT.MouseUp, SWT.MouseWheel, SWT.MouseDoubleClick, SWT.MenuDetect, SWT.DragDetect,
		SWT.MouseEnter, SWT.MouseExit, SWT.MouseMove,
		SWT.Help
		)
	private var nonModifierKeyPressed = false

	type GeometryChangedListener = Function[ViewCanvas,Unit]
	val geometryChangedListeners = new mutable.HashSet[GeometryChangedListener]

	val app: Application
	var model: Group = new Group
	var instance = model.instance(RootDataSelection, Map.empty)

	val ui = new UIState(this)
	val policy = new EditingStatePolicy(this)
	val cellEdit = new GridTextEditor(this)

	val popup = new MenuManager(new Menu(this.getShell, SWT.POP_UP))
	popup.add(app.actions.GroupRowCols)
	popup.add(app.actions.HideLabel)
	popup.add(app.actions.ShowHiddenLabels)
	popup.addSeparator
	popup.add(app.actions.DeleteRowCols)
	popup.add(app.actions.DeleteAxis)
	
	final val pixelFactor = 256
	final val unitScale = 1F / pixelFactor
	final val minScale = 0.2F * unitScale
	final val maxScale = 5F * unitScale
	var scale = unitScale

	var idealOffsetX = 0F
	var idealOffsetY = 0F
	var offsetX = 0
	var offsetY = 0
	var minX = 0
	var minY = 0
	var maxX = 0
	var maxY = 0

	val hiddenCursor = {
		// create a cursor with a transparent image
		val display = getDisplay
		val white = display.getSystemColor(SWT.COLOR_WHITE);
		val black = display.getSystemColor(SWT.COLOR_BLACK);
		val palette = new PaletteData(Array(white.getRGB(), black.getRGB()))
		val sourceData = new ImageData(16, 16, 1, palette);
		sourceData.transparentPixel = 0;
		new Cursor(display, sourceData, 0, 0);
	}


	addDisposeListener((e: DisposeEvent) => {
			hiddenCursor.dispose})
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
		val state = new EventState(e)
		val point = viewToModel(new Point(e.x, e.y))

		def clickNoDrag(item: Selectable) {
			val alreadySelected = (item == ui.selection)
			if (!alreadySelected) ui.select(item)
			if (item.isInstanceOf[SingleGridSelection]) beginCellEdit(None)
		}
		e.`type` match {
			// Events which fire regardless of widget:
			case SWT.MouseWheel =>
				if (state.isAltBehaviour) {
					val oldScale = scale
					val newScale = quantiseScale(minScale max power(oldScale, 1.05F, e.count) min maxScale)
					if (oldScale != newScale) {
						// 1:1 scale is special. If the zoom 'crosses' 1:1, snap to exactly 1:1
						scale = if ((oldScale < unitScale && newScale > unitScale) || (oldScale > unitScale && newScale < unitScale))
								unitScale
							else
								newScale
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
				val hitThing = selectableThingAt(point)
				if (state.isPrimaryButton) {
					if (state.isExtendSelect)
						ui.extendSelectionTo(hitThing)

					else if (state.isMultiSelect)
						ui.toggle(hitThing)

					else getDraggable(hitThing, point) match {
						case NoDrag =>
							clickNoDrag(hitThing)
						case draggable =>
							ui.focus = hitThing
							ui.dragState = draggable
					}

				} else {
					// If mouse click is not primary button, select the thing under
					// the mouse, unless it is already selected, (in which case the
					// user may wish to invoke a popup menu on it, so don't change selection).
					if (!ui.selection.contains(hitThing)) {
						endCellEdit
						ui.select(hitThing)
					}
				}
					
			case SWT.MenuDetect =>
				// MenuDetect events are in Display coordinates, not control coordinates.
				// (But they're rare, so don't mind the slight redundancy.)
				val point = viewToModel(this.toControl(e.x, e.y))
				val item = selectableThingAt(point)
				if (item.isInstanceOf[LabelInstance]){
					popup.menu.setLocation(e.x, e.y)
					popup.menu.setVisible(true)
				}

			case SWT.MouseUp =>
				ui.dragState match {
					case drag: ConcreteDragObject =>
						if (drag.isStarted)
							drag.commit
						else
							clickNoDrag(ui.focus)
					case _ =>
				}
				ui.dragState = NoDrag
		
			case SWT.MouseMove =>
				setCursor(null)
				ui.dragState match {
					case NoDrag =>
						val isDragSelect = state.isPrimaryButton
						if (isDragSelect) {
							val hitThing = selectableThingAt(point)
							ui.extendSelectionTo(hitThing)
						}
					case d: ConcreteDragObject =>
						d.dragTo(point)
					redraw
				}

			case SWT.DragDetect =>
				ui.dragState match {
					case drag: ConcreteDragObject if !drag.isStarted => drag.start
					case _ =>
				}
			
			case SWT.MouseExit =>
			case SWT.MouseEnter=>
			case SWT.KeyDown =>
				// Hide mouse pointer if user presses a non-modifier key. 
				// Ignore repeats.
				if ((e.keyCode & SWT.MODIFIER_MASK) == 0 && ! nonModifierKeyPressed) {
					this.setCursor(hiddenCursor)
					nonModifierKeyPressed = true
				}

				if (e.keyCode == SWT.MOD3) ui.selectLargeBits = true
				processKey(e)
			
			case SWT.KeyUp =>
				if ((e.keyCode & SWT.MODIFIER_MASK) == 0) {
					nonModifierKeyPressed = false
				}
				if (e.keyCode == SWT.MOD3) ui.selectLargeBits = false

			case SWT.FocusOut =>
				// TODO: get rid of any overlays
				
			case _ => /* ignore */;
		}
	}

	private def scrollHorizontal(e: Event){
		val unscaled = getHorizontalBar.getSelection
		val x = (unscaled / 256F) / scale + minX
		if (x != offsetX)
			setOrigin(x, offsetY)
	}
	private def scrollVertical(e: Event){
		val unscaled = getVerticalBar.getSelection
		val y = (unscaled / 256F) / scale + minY
		if (y != offsetY)
			setOrigin(offsetX, y)
	}

	// Returns 'unquantised' rounded to the nearest 1/256th... Unnecessary? Remove?
	private def quantiseScale(unquantised: Float) = unquantised

	private def power(result: Float, d: Float, e: Int): Float =
		if (e == 0)
			result
		else if (e < 0)
			power(result/d, d, e+1)
		else
			power(result*d, d, e-1)


	def selectableThingAt(point: Point): Selectable = {
		instance.hitTest(point)
	}


	/**
	 * If the model (table) size, or zoom factor, or view (control) size, has changed,
	 * we need to recompute the view area available and scroll position. Et voila.
	 * Provides the limits for scrolling, and updates the scrollbars.
	 * Do not confuse with (SWT-compatible) method 'computeSize'.
	 */
	def computeBounds {
		val clientArea = getClientArea
		val canvasWidth = clientArea.width
		val canvasHeight = clientArea.height
		val modelSize = model.size
		val modelWidth = Math.ceil(modelSize.x * scale).toInt
		val modelHeight = Math.ceil(modelSize.y * scale).toInt

		val differenceWidth = modelSize.x - (canvasWidth / scale).toInt
		if (modelWidth > canvasWidth){
			minX = 0
			maxX = differenceWidth
		}else{
			minX = 0
			maxX = 0
		}

		val differenceHeight = modelSize.y - (canvasHeight / scale).toInt
		if (modelHeight > canvasHeight){
			minY = 0
			maxY = differenceHeight
		}else{
			minY = 0
			maxY = 0
		}

		updateOrigin(true)
		val xScroll = getHorizontalBar
		val xPage = (canvasWidth * 256).toInt
		val xSize = ((canvasWidth max modelWidth) * 256).toInt
		val xSmall = 20*256
		xScroll.setValues(
			((offsetX - minX) * scale * 256F).toInt, // selection
			0, // minimum
			xSize, // maximum
			xPage, // thumb
			xSmall, // increment
			(xPage - xSmall) max xSmall) // pageIncrement

		val yScroll = getVerticalBar
		val yPage = (canvasHeight * 256).toInt
		val ySize = ((canvasHeight max modelHeight) * 256).toInt
		val ySmall = 20*256
		yScroll.setValues(
			((offsetY - minY) * scale * 256F).toInt, // selection
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

		val x = minX max idealOffsetX.round.toInt min maxX
		val y = minY max idealOffsetY.round.toInt min maxY
		if (x != offsetX || y != offsetY || scaleChanged) {
			offsetX = x; offsetY = y
			redraw
			updateListeners
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

	def modelToView(r: Rectangle) = new Rectangle(
			((r.x - offsetX) * scale).toInt,
			((r.y - offsetY) * scale).toInt,
			(r.width * scale).toInt,
			(r.height * scale).toInt)

	def withClipRoundRightAndDown(d: DrawingContext, rect: Rectangle)(op: => Unit) {
		val rightPixel = Math.ceil(((rect.x + rect.width) - offsetX) * scale)
		val bottomPixel = Math.ceil(((rect.y + rect.height) - offsetY) * scale)
		val rightRounded = Math.ceil(rightPixel / scale + offsetX).toInt
		val bottomRounded = Math.ceil(bottomPixel / scale + offsetY).toInt
		d.withclip(new Rectangle(rect.x, rect.y, rightRounded - rect.x, bottomRounded - rect.y))(op)
	}


	
    def paintControl(e: PaintEvent) {
		val gc = e.gc
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)
		val gfx = new DrawingContext(gc, ui) {
			override val pixelDistance = (1 / scale).ceil.toInt
		}
		try{
			val canvasArea = getClientArea
			val gridSize = model.size

			val trans = gfx.newTransform
			trans.identity
			trans.scale(scale.toFloat, scale.toFloat)
			trans.translate(- offsetX.toFloat, - offsetY.toFloat)
			gc.setTransform(trans)

			gc.setBackground(gc.getDevice.getSystemColor(SWT.COLOR_GRAY))
			gc.fillRectangle(0, 0, gridSize.x, gridSize.y)

			model.render(gfx, Origin, Map.empty)

			trans.identity
			gc.setTransform(trans)
			for(d <- ui.dropTarget)
				d.render(gfx, Origin, Map.empty)

		}catch{
			case ex => {println(ex); throw ex}

		}finally{
			gfx.dispose
		}
    }


	def processKey(e: Event){
		val shift = (e.stateMask & SWT.SHIFT) != 0
		val potentialCellEdit = !ui.fineEditMode &&
								ui.selection.isInstanceOf[SingleGridSelection]
		e.keyCode match {
			case SWT.ESC if ui.dragState.isStarted =>
				ui.dragState.cancel
				ui.dragState = NoDrag
			
			case SWT.ARROW_UP => moveSelection(Up, ByOne)
			case SWT.ARROW_DOWN => moveSelection(Down, ByOne)
			case SWT.ARROW_LEFT => moveSelection(Left, ByOne)
			case SWT.ARROW_RIGHT => moveSelection(Right, ByOne)
			case SWT.TAB if shift => moveSelection(Left, ByOne)
			case SWT.TAB if !shift => moveSelection(Right, ByOne)
			case SWT.DEL if ui.selection.isInstanceOf[LabelSelection] =>
				// Hack: ignore keypress to allow it to work as menu shortcut.
				e.doit = false
				
			case SWT.DEL | SWT.BS if potentialCellEdit => beginCellEdit(Some(""))
			case _ if potentialCellEdit && !e.character.isControl => beginCellEdit(Some(e.character.toString))
			case SWT.CR | SWT.KEYPAD_CR if potentialCellEdit => beginCellEdit(None)
			case _ => //ignore
		}
	}

	def beginCellEdit(s: Option[String]) {
		ui.selection match {
			case sel: SingleGridSelection =>
				autoVivifySelection(sel)
				ui.fineEditMode = true
				for(str <- s) {
					cellEdit.input.setText(str)
					val p = str.length
					cellEdit.input.setSelection(p, p)
				}
			case _ => error("Selection not a single cell.")
		}
	}

	def endCellEdit { ui.fineEditMode = false }

	/**
	 * If selection is off end of data, expand the axes to fit.
	 */
	def autoVivifySelection(sel: SingleGridSelection) {
		if (!sel.withinData){
			sel match {
				case lab: LabelInstance =>
					val (ax, ix) = (lab.axis, lab.index)
					assert(ix == ax.length)
					ax.insert(ix, 1)

				case cel: AbstractCellInstance =>
					for((ax, ix) <- sel.coords) {
						assert(ix >= 0 && ix <= ax.length)
						if (ix == ax.length)
							ax.insert(ix, 1)
					}
			}
			assert(sel.withinData)
		}
	}

	def getDraggable(hit: Selectable, hitPoint: Point): DragObject = hit match {
		case t: Table#Instance => new GroupElementDrag(t, hitPoint)
		case _ => NoDrag
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
			case SWT.ARROW_UP => moveSelection(Up, ByOne); true
			case SWT.ARROW_DOWN => moveSelection(Down, ByOne); true
			case SWT.ARROW_LEFT if atLeft => moveSelection(Left, ByOne); true
			case SWT.ARROW_RIGHT if atRight => moveSelection(Right, ByOne); true
			case SWT.TAB if shift => moveSelection(Left, ByOne); true
			case SWT.TAB if !shift => moveSelection(Right, ByOne); true

			case SWT.ESC => ui.fineEditMode = false; true
			case SWT.CR | SWT.KEYPAD_CR => ui.fineEditMode = false; true
			case _ => false
		}
	}

	def moveSelection(direction: CompassPosition, granularity: MovementGranularity){
		ui.selection match {
			case sel: SingleGridSelection =>
				implicit val hints: SelectionHints = ui.selectionHintsFor(direction.orientation)
				sel.adjacent(direction) match {
					case None => getDisplay.beep
					case Some(newSel) =>
						ui.select(newSel)
						ui.selectionHints = hints
				}
			case _ => // ignore
		}
	}


	/**
	 * Deliver the preferred size of the control. Assumes that the model already
	 * has an available computed size.
	 */
    override def computeSize(wHint: Int, hHint: Int, changed: Boolean) = {
		try{
			val clientSize = model.size
			val preferredX = Math.max((clientSize.x * scale).toInt, 300)
			val preferredY = Math.max((clientSize.y * scale).toInt, 200)
			val idealSize = computeTrim(0, 0, preferredX, preferredY)
			new Point(
				(if (wHint != SWT.DEFAULT) wHint else idealSize.width),
				(if (hHint != SWT.DEFAULT) hHint else idealSize.height))

		}catch{
			case ex => {println(ex); throw ex}
		}
    }


}

