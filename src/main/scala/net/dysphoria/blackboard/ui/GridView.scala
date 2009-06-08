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
	var originX = 0.0F
	var originY = 0.0F

	addDisposeListener((e: DisposeEvent) => {
			// Not much to do
						})
    addPaintListener(paintControl _)
	
	private val listener: Listener = handleEvent _
	Array(
		SWT.KeyDown, SWT.KeyUp,
		SWT.MouseDown, SWT.MouseUp, SWT.MouseWheel, SWT.MouseDoubleClick,
		SWT.MouseEnter, SWT.MouseExit, SWT.MouseMove,
		SWT.Help

	) foreach(evt => addListener(evt, listener))

	private def handleEvent(e: Event) {
		val state = new EventState(e.stateMask)
		val x = (e.x/scale + originX).toInt
		val y = (e.y/scale + originY).toInt

		e.`type` match {
			case SWT.MouseDown => {
				val hitThing = selectableThingAt(x, y)
				if (state.isExtendSelect)
					ui.extendSelectionTo(hitThing)

				else if (state.isMultiSelect)
					ui.toggle(hitThing)

				else {
					if (ui.selection contains hitThing){ // potential drag
						ui.focus = hitThing
						ui.dragAnchor = Some(new Point(0, 0)) // TODO
						ui.dragState = MouseDown // Doesn't /really/ start til mouse moves.
						
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
							val hitThing = selectableThingAt(x, y)
							ui.extendSelectionTo(hitThing)
						}
						//mouseX = e.x; mouseY = e.y
					}
					case MouseDown => ui.dragState = Dragging
					case Dragging => println("draaaaag");// TODO
				}
			}
			case SWT.MouseExit => {mouseX = -1; redraw}
			case SWT.MouseEnter=> ;
			case SWT.MouseWheel=> if (state.isAltBehaviour) {
				val newScale = (0.2F max power(scale, 1.05F, e.count) min 5.0F)
				if (scale != newScale) {
					originX = x - (e.x / newScale)
					originY = y - (e.y / newScale)
					scale = newScale
					redraw
					e.doit = false // consume the event
				}
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

	def power(result: Float, d: Float, e: Int): Float =
		if (e == 0)
			result
		else if (e < 0)
			power(result/d, d, e+1)
		else
			power(result*d, d, e-1)

	def selectableThingAt(x: Int, y: Int): Selectable =
		everything.hitTest(new Point(x, y)) match {
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
					SingleGridSpace(everything, ix, iy)
					
				}else everything(ix, iy) match {
					case b: DimensionLabelsBlock => {
						if (coordsX.isDefined && coordsY.isDefined){
							SingleDimensionLabel(b, b.displayToTableCoordinate(
									coordsX.get._1,
									coordsY.get._1))
						}else
							NullSelection
					}
					case b: TableBlock => {
						if (coordsX.isDefined && coordsY.isDefined){
							SingleTableCell(b, b.displayToTableCoordinates(
									coordsX.get._1,
									coordsY.get._1))
						}else
							NullSelection

					}
					case b => SingleGridSpace(everything, ix, iy)
				}
			}
			case None => NullSelection
		}
		

	def hit(dds: List[DisplayDimension], p: Int)={
		DisplayDimension.hitTest(dds, p) match {
			case None => "none"
			case Some((coords, offset)) => coords.toString + "+" + offset
		}
	}
    
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
				trans.translate(- originX.toFloat, - originY.toFloat)
				gc.setTransform(trans)

				gc.setBackground(gfx.colorForRGB(white))
				gc.fillRectangle(0, 0, gridSize.x, gridSize.y)
				everything.render(gfx, Origin)

				trans.identity
				gc.setTransform(trans)
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
