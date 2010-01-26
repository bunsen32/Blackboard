package net.dysphoria.blackboard

import org.eclipse.swt
import swt.layout
import swt.graphics
import swt.widgets.{Display, Shell, Menu, MenuItem}
import swt.SWT
import swt.events._

import ui._
import ui.model._
import ui.Listeners._
import ui.Origin._

/**
 * Says “hello” to the world.
 */
object Test {
	var display: Display = _
	
	def setUpApp {
		Display setAppName "Blackboard"
		display = new Display()
	}

	def testWindow: Shell = {
		val shell = new Shell(display)
		shell.setText("Blackboard")
		val shellLayout = new layout.FillLayout()
		shellLayout.marginWidth = 0
		shellLayout.marginHeight = 0
		shell.setLayout(shellLayout)

/*
		val hairLine = Some(new LineDescriptor(new graphics.RGB(0, 0, 100), 0.05F))
		val lightLine = Some(new LineDescriptor(new graphics.RGB(0, 0, 0), 0.333333F))
		val heavyLine = Some(new LineDescriptor(new graphics.RGB(0, 0, 0), 0.88888888F))

		// One big array of data:
		val axis1 = new ArrayAxis{length=10; interItemLine = heavyLine}
		val axis2 = new ArrayAxis{length=10; interItemLine = lightLine}
		val axis3 = new ArrayAxis{length=2; interItemLine = lightLine}
		val axis4 = new ArrayAxis{length=3; interItemLine = hairLine}
		val n = None
		val block = new TableArrayData{
			xAxes = Seq(axis1, axis3)
			yAxes = Seq(axis2, axis4)
			array = new TestDataArray(Array(axis1, axis2, axis3, axis4))
		}

		// List of 10 items, and a total:
		val axisA = new ArrayAxis{length=10; interItemLine = lightLine}
		val axisS = new StructAxis{elements = List("Data", "Total"); interItemLine = heavyLine}
		val data = new TableArrayData {
			xAxes = Nil
			yAxes = Seq(axisA)
			array = new TestDataArray(Array(axisA))
		}
		val total = new TableArrayData {
			xAxes = Nil
			yAxes = Nil
			array = new TestDataArray(Array())
		}
		val block = new TableStruct {
			orientation = Vertical
			structAxis = axisS
			xAxes = Nil
			yAxes = Seq(axisS)
			elements = Seq(data, total)
		}
*/
/*		val axisS1 = new StructAxis{elements = List("Boh", "Bloo", "Blah"); interItemLine = heavyLine}
		val axisA1 = new ArrayAxis{length=3; interItemLine = hairLine}
		val axisA2 = new ArrayAxis{length=3; interItemLine = lightLine}
		val axisA3 = new ArrayAxis{length=4; interItemLine = lightLine}

		val inner1 = new TableArrayData {
			xAxes = Nil
			yAxes = Seq(axisA1)
			array = new TestDataArray(Array(axisA2, axisA3, axisA1))
		}
		val inner2 = new TableArrayData {
			xAxes = Nil
			yAxes = Nil
			array = new TestDataArray(Array(axisA2, axisA3))
		}
		val inner3 = new TableArrayData {
			xAxes = Nil
			yAxes = Nil
			array = new TestDataArray(Array(axisA2, axisA3))
		}
		val block = new TableStruct {
			orientation = Horizontal
			structAxis = axisS1
			xAxes = Seq(axisS1, axisA2)
			yAxes = Seq(axisA3)
			elements = Seq(inner1, inner2, inner3)
		}
*/
		val app0 = new Application {
			var currentView: ui.ViewCanvas = null
		}

		val table0 = new ui.model.Table(null)
		val block = new TableArrayData(table0) {
			xAxes = Nil
			yAxes = Nil
		}
		table0.topBlock = block
		
		val view = new ui.ViewCanvas(shell, SWT.NONE) {
			val app = app0
			model.add(Origin, table0)
		}
		view.model.computeSize
		app0.currentView = view

		val menu = new Menu(shell, SWT.BAR)
		shell setMenuBar menu
		val fileMenuItem = new MenuItem(menu, SWT.CASCADE)
		fileMenuItem setText "File"
		val editMenuItem = new MenuItem(menu, SWT.CASCADE)
		editMenuItem setText "Edit"
		val tableMenuItem = new MenuItem(menu, SWT.CASCADE)
		tableMenuItem setText "Table"
		val tableMenu = new actions.MenuManager(tableMenuItem)
		tableMenu.add(app0.actions.GroupRowCols)
		tableMenu.add(app0.actions.HideLabel)
		tableMenu.add(app0.actions.ShowHiddenLabels)
		tableMenu.addSeparator
		tableMenu.add(app0.actions.RepeatRowCol)
		tableMenu.add(app0.actions.InsertSimilarRowCol)
		tableMenu.add(app0.actions.InsertDistinctRowCol)
		tableMenu.addSeparator
		tableMenu.add(app0.actions.DeleteRowCols)
		tableMenu.add(app0.actions.DeleteAxis)

		shell.pack
		shell.open
		shell
	}
	
	def main(args: scala.Array[String]) {
		try{
			setUpApp
			val window = testWindow

			while(!window.isDisposed){
				if (!display.readAndDispatch) display.sleep
			}
			display.dispose
			
		}catch{
			case ex => println(ex.printStackTrace)
		}
	}
}

