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

class GridView(parent: Composite, style: Int) extends Canvas(parent, style) {
    private val Origin = new Point(0, 0)
    val white = new RGB(255, 255, 255)
	val black = new RGB(0, 0, 0)
	val red = new RGB(255, 0, 0)

	val everything = new MetaGrid
	val selector = new Selector(this)
	var mouseX = -1
	var mouseY = 0
	var scale = 1.0
	var originX = 0.0
	var originY = 0.0

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
		val x = (e.x/scale - originX).toInt
		val y = (e.y/scale - originY).toInt

		e.`type` match {
			case SWT.MouseDown => {
				val hitThing = selectableThingAt(x, y)
				if (state.isExtendSelect)
					selector.extendTo(hitThing)

				else if (state.isMultiSelect)
					selector.toggle(hitThing)

				else
					selector.select(hitThing)
			}
			case SWT.MouseUp => {
				// STOP DRAG
			}
			case SWT.MouseMove => {
				val isDragging = state.isPrimaryButton
				if (isDragging) {
					val hitThing = selectableThingAt(x, y)
					selector.extendTo(hitThing)
				}
			}
			case SWT.MouseExit => {mouseX = -1; redraw}
			case SWT.MouseEnter=> ;
			case SWT.MouseWheel=> {
				val newScale = (0.2 max power(scale, 1.05, e.count) min 2.0)
				if (scale != newScale) {
					originX -= x - (e.x / newScale - originX)
					originY -= y - (e.y / newScale - originY)
					scale = newScale
					redraw
				}
			}
			case SWT.KeyDown => {
				println(e.keyCode.toHexString)
			}
			case _ => /* ignore */;
		}
	}

	def power(result: Double, d: Double, e: Int): Double =
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

				everything(ix, iy) match {
					case Some(b: DimensionLabelsBlock) => {
						if (coordsX.isDefined && coordsY.isDefined){
							SingleDimensionLabel(b, b.displayToTableCoordinate(
									coordsX.get._1,
									coordsY.get._1))
						}else
							SingleDisplayBlock(b)

					}
					case Some(b: TableBlock) => {
						if (coordsX.isDefined && coordsY.isDefined){
							SingleTableCell(b, b.displayToTableCoordinates(
									coordsX.get._1,
									coordsY.get._1))
						}else
							SingleDisplayBlock(b)

					}
					case None => EmptyGridSpace(ix, iy)
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
		val gfx = new Gfx(gc)
		try{
			val sz = getSize

			gfx.withclip(new Rectangle(0, 0, sz.x, sz.y)){
				val trans = gfx.newTransform
				trans.identity
				trans.scale(scale.toFloat, scale.toFloat)
				trans.translate(originX.toFloat, originY.toFloat)
				
				gc.setTransform(trans)
				gc.setBackground(gfx.colorForRGB(white))
				gc.fillRectangle(0, 0, sz.x, sz.y)
				everything.render(gfx, Origin)

				trans.identity
			}
			if (mouseX != -1){
				gc.setForeground(gfx.colorForRGB(red))
				gc.drawLine(mouseX, 0, mouseX, sz.y)
				gc.drawLine(0, mouseY, sz.x, mouseY)
			}

		}catch{
			case ex => {println(ex); throw ex}

		}finally{
			gfx.dispose
		}
    }
    
    override def computeSize(wHint: Int, hHint: Int, changed: Boolean) = {
		try{
			var size = everything.size
			new Point(
				(if (wHint != SWT.DEFAULT) wHint else size.x),
				(if (hHint != SWT.DEFAULT) hHint else size.y))
		  
		}catch{
			case ex => {println(ex); throw ex}
		}
    }
    
}
