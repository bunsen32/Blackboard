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

		val app0 = new Application {
			var currentView: ui.ViewCanvas = null
		}

		val table0 = new ui.model.Table(new TableArrayData)
		
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
		tableMenu.add(app0.actions.NewTable)
		tableMenu.addSeparator
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

