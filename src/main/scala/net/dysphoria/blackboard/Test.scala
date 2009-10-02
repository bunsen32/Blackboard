package net.dysphoria.blackboard

import org.eclipse.swt
import swt.layout
import swt.graphics
import swt.widgets
import swt.SWT._
import swt.events._

import blackboard.data._
import blackboard.gfx._
import blackboard.ui._
import Listeners._

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
		val shellLayout = new layout.FillLayout()
		shellLayout.marginWidth = 0
		shellLayout.marginHeight = 0
		shell.setLayout(shellLayout)

		val hairLine = Some(new LineDescriptor(new graphics.RGB(0, 0, 100), 0.05F))
		val lightLine = Some(new LineDescriptor(new graphics.RGB(0, 0, 0), 0.333333F))
		val heavyLine = Some(new LineDescriptor(new graphics.RGB(0, 0, 0), 0.88888888F))
		
		/*
			val axis1 = new ArrayAxis{length=10; interItemLine = heavyLine}
			val axis2 = new ArrayAxis{length=10; interItemLine = lightLine}
			val axis3 = new ArrayAxis{length=2; interItemLine = lightLine}
			val axis4 = new ArrayAxis{length=3; interItemLine = hairLine}
			val n = None
			val ab = new ArrayBlock{
				xAxes = Seq(axis1, axis3)
				yAxes = Seq(axis2, axis4)
				array = new TestArray(Array(axis1, axis2, axis3, axis4))
			}
			view.table = Some(ab)
			ab.computeSize
		*/
		val axisA = new ArrayAxis{length=10; interItemLine = lightLine}
		val axisS = new StructAxis{elements = List("Data", "Total"); interItemLine = heavyLine}
		val data = new ArrayBlock {
			xAxes = Nil
			yAxes = Seq(axisA)
			array = new TestArray(Array(axisA))
		}
		val total = new ArrayBlock {
			xAxes = Nil
			yAxes = Nil
			array = new TestArray(Array())
		}
		val block = new StructBlock {
			orientation = YOrientation
			structAxis = axisS
			xAxes = Nil
			yAxes = Seq(axisS)
			elements = Seq(data, total)
		}
		block.computeSize
		val view = new ui.ViewCanvas(shell, NONE) {
			val table = block
		}

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

