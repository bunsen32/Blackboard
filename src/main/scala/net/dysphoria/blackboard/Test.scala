package net.dysphoria.blackboard

import org.eclipse.swt
import swt.layout
import swt.graphics
import swt.widgets
import swt.SWT._

import blackboard.data._
import blackboard.gfx._
import blackboard.ui._

/**
 * Says “hello” to the world.
 */
object Test {
	var display: widgets.Display = _
	
	def setUpApp {
		widgets.Display setAppName "Blackboard"
		display = new widgets.Display()
	}

	def testWindow: widgets.Shell = {
		val shell = new widgets.Shell(display)
		shell.setText("Blackboard")
		shell.setLayout(new layout.GridLayout(1, true))

		val view = new ui.GridView(shell, swt.SWT.NONE)
		val grid = view.everything


		val d0 = new ConcreteIntegerRangeDimension(1 to 10)
		val dd0 = new DisplayDimension(d0)
		dd0.defaultItemWidth = 30
		dd0.interItemLine = Some(new LineDescriptor(new graphics.RGB(40, 40, 40), 1))

		val d1 = new ConcreteIntegerRangeDimension(1 to 10)
		val dd1 = new DisplayDimension(d1)
		dd1.defaultItemWidth = 26
		dd1.interItemLine = Some(new LineDescriptor(new graphics.RGB(40, 40, 40), 2))

		val d2 = new ConcreteIntegerRangeDimension(1 to 2)
		val dd2 = new DisplayDimension(d2)
		dd2.interItemLine = Some(new LineDescriptor(new graphics.RGB(160, 160, 160), 1))
		dd2.defaultItemWidth = 26

		val dFill = new DisplayDimension(UnitDimension)
		dFill.defaultItemWidth = 30


		val t = new ByValueTable {
			val arity = 3
			val dimensions = Array(d0, d1, d2)
			def apply(args: Seq[Any]) = (1 /: args)(_ * _.asInstanceOf[Int])
		}
		val tb = new TableBlock(grid, t)
		tb.dimensionMap = Map(dd0 -> 0, dd1 -> 1, dd2 -> 2)


		val tSum = new ByValueTable {
			val arity = 2
			val dimensions = Array(d1, d2)
			def apply(args: Seq[Any]) =
				(0 /: d0.range)((sum, a) => sum + (t.apply(
						List(a) ++ args)))
		}
		val tbSum = new TableBlock(grid, tSum)
		tbSum.dimensionMap = Map(dd1 -> 0, dd2 -> 1)

		grid.setGridSize(4, 2)
		grid.xDimensionLists(0) = List(dFill)
		grid.xDimensionLists(1) = List(dFill)
		grid.xDimensionLists(2) = List(dd0)
		grid.xDimensionLists(3) = List(dFill)

		grid.yDimensionLists(0) = List(dFill)
		grid.yDimensionLists(1) = List(dd1, dd2)

		grid(2, 0) = Some(new DimensionLabelsBlock(grid, dd0))

		grid(0, 1) = Some(new DimensionLabelsBlock(grid, dd1))
		grid(1, 1) = Some(new DimensionLabelsBlock(grid, dd2))
		grid(2, 1) = Some(tb)
		grid(3, 1) = Some(tbSum)

		shell.pack
		shell.open
		shell
	}
	
	
	def main(args: Array[String]) {
		try{
			setUpApp
			val window = testWindow

			val img = display getSystemImage ICON_WARNING
			val popup = new widgets.Shell(window, NO_TRIM | ON_TOP)
			popup.setBackground(display getSystemColor COLOR_RED)
			/*val region = new graphics.Region(display)
			val pixel = new graphics.Rectangle(0, 0, 1, 1)
			for(y <- 0 to 200 by 2; x <- 0 to 200 by 2){
				pixel.x = x; pixel.y = y
				region add pixel
			}
			popup setRegion region
			val size = region.getBounds
				println(size.x + ","+size.y)
			*/
			popup setAlpha 128

			popup setSize(200, 200)
			import Listeners._
			popup addPaintListener ((e: swt.events.PaintEvent) => {
				val bounds = img getBounds
				val size = popup.getSize
				e.gc.drawImage(img, 0, 0, bounds.width, bounds.height, 10, 10, size.x-20, size.y-20)
			})
			popup.addListener(KeyDown, ((e: widgets.Event) => {
						if (e.character == ESC) popup setVisible false
					}))

			window.addMouseListener(new swt.events.MouseAdapter {
				override def mouseUp(e: swt.events.MouseEvent) {
					popup.open
				}
				})

			while(!window.isDisposed){
				if (!display.readAndDispatch) display.sleep
			}
			display.dispose
			println("okay")
			
		}catch{
			case ex => println(ex.printStackTrace)
		}
	}
}

