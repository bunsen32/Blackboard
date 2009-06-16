package net.dysphoria.blackboard.ui

import scala.collection.mutable
import org.eclipse.swt
import swt.SWT
import swt.events._
import swt.graphics._
import swt.widgets.{List=>_, _}
import Listeners._
import blackboard.gfx._
import selection._

class GridView(parent: Composite, style: Int) extends Canvas(parent, SWT.H_SCROLL|SWT.V_SCROLL) {
    private val Origin = new Point(0, 0)
    val white = new RGB(255, 255, 255)
	val black = new RGB(0, 0, 0)
	val red = new RGB(255, 0, 0)

	val everything = new MetaGrid
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
	getHorizontalBar.addListener(SWT.Selection, scrollHorizontal _)
	getVerticalBar.addListener(SWT.Selection, scrollVertical _)

	private def handleEvent(e: Event) {
		val state = new EventState(e.stateMask)
		val point = viewToModel(new Point(e.x, e.y))

		e.`type` match {
			case SWT.Resize => computeBounds
			case SWT.MouseDown => {
				val (hitThing, remainder) = selectableThingAndRemainderAt(point)
				if (state.isExtendSelect)
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
				}
			}
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
						case b: SingleGridSpace => DragOperation.start(new BlockDragClient(this))
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
		val (thing, remainder) = selectableThingAndRemainderAt(point)
		thing
	}
	def selectableThingAndRemainderAt(absolute: Point): (Selectable, Point) = {
		everything.hitTest(absolute) match {
			case Some((ix, iy, pt)) => {
				val dimListX = everything.xDimensionLists(ix)
				val coordsX = DisplayDimension.hitTest(dimListX, pt.x)

				val dimListY = everything.yDimensionLists(iy)
				val coordsY = DisplayDimension.hitTest(dimListY, pt.y)

				val clickedOnSelectedBlock = ui.selection match {
					case s: IsBlockLevel if s.blocks contains everything(ix, iy) =>	true
					case _ => false
				}
				if (clickedOnSelectedBlock || ui.selectLargeBits) {
					(SingleGridSpace(everything, ix, iy), pt)
					
				}else if (coordsX.isDefined && coordsY.isDefined){
					val x = coordsX.get
					val y = coordsY.get
					val remainder = new Point(x._2, y._2)
					everything(ix, iy) match {
						case b: DimensionLabelsBlock =>
							(SingleDimensionLabel(b, 
								 b.displayToTableCoordinate(x._1, y._1)), remainder)

						case b: TableBlock =>
							(SingleTableCell(b, 
								b.displayToTableCoordinates(x._1, y._1)), remainder)

						case b => (SingleGridSpace(everything, ix, iy), pt)
					}
				}else
					(NullSelection, absolute)
			}
			case None => (NullSelection, absolute)
		}
	}
		

	def hit(dds: List[DisplayDimension], p: Int)={
		DisplayDimension.hitTest(dds, p) match {
			case None => "none"
			case Some((coords, offset)) => coords.toString + "+" + offset
		}
	}
	
	/**
	 * If the physical size, or zoom factor, or window size, has changed,
	 * we need to recompute the view area available and scroll position. Et voila:
	 */
	def computeBounds {
		val canvas = getClientArea
		canvas.width = (canvas.width / scale).toInt
		canvas.height = (canvas.height / scale).toInt
		val model = everything.size
		val x0 = model.x - canvas.width
		val y0 = model.y - canvas.height
		
		minX = 0 min x0
		minY = 0 min y0
		maxX = 0 max x0
		maxY = 0 max y0

		val rangeX = maxX - minX
		val rangeY = maxY - minY

		updateOrigin
		val xScroll = getHorizontalBar
		val xPage = ((canvas.width min model.x) * 256).toInt
		val xSmall = 20*256
		xScroll.setValues(
			((offsetX - minX) * 256F).toInt, // selection
			0, // minimum
			((canvas.width max model.x) * 256).toInt, // maximum
			xPage, // thumb
			xSmall, // increment
			(xPage - xSmall) max xSmall) // pageIncrement
		
		val yScroll = getVerticalBar
		val yPage = ((canvas.height min model.y) * 256).toInt
		val ySmall = 20*256
		yScroll.setValues(
			((offsetY - minY) * 256F).toInt, // selection
			0, // minimum
			((canvas.height max model.y) * 256).toInt, // maximum
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

	def setOrigin(x: Float, y: Float){
		idealOffsetX = x
		idealOffsetY = y
		updateOrigin
	}

	def updateOrigin {
		val x = minX.toFloat max idealOffsetX min maxX
		val y = minY.toFloat max idealOffsetY min maxY
		if (x != offsetX || y != offsetY) {
			offsetX = x; offsetY = y
			redraw
		}
	}

	def viewToModel(p: Point) = new Point(
		(p.x / scale + offsetX).toInt,
		(p.y / scale + offsetY).toInt)

	def modelToView(p: Point) = new Point(
		((p.x - offsetX) * scale).toInt,
		((p.y - offsetY) * scale).toInt)

    def paintControl(e: PaintEvent) {
		val gc = e.gc
		gc.setAdvanced(true)
		gc.setAntialias(SWT.ON)
		val gfx = new DrawingContext(gc, ui)
		try{
			val canvasArea = getClientArea
			val gridSize = everything.size

			gfx.withclip(new Rectangle(0, 0, canvasArea.width, canvasArea.height)){
				val trans = gfx.newTransform
				trans.identity
				trans.scale(scale.toFloat, scale.toFloat)
				trans.translate(- offsetX.toFloat, - offsetY.toFloat)
				gc.setTransform(trans)

				gc.setBackground(gfx.colorForRGB(white))
				gc.fillRectangle(0, 0, gridSize.x, gridSize.y)
				
				everything.render(gfx, Origin)

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
    
    override def computeSize(wHint: Int, hHint: Int, changed: Boolean) = {
		try{
			val clientSize = everything.size
			val idealSize = computeTrim(0, 0, clientSize.x, clientSize.y)
			new Point(
				(if (wHint != SWT.DEFAULT) wHint else idealSize.width),
				(if (hHint != SWT.DEFAULT) hHint else idealSize.height))
		  
		}catch{
			case ex => {println(ex); throw ex}
		}
    }
    
}
