/*
 * GridViewDragClient.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

abstract class GridViewDragClient(override val control: GridView) extends DragClient {
	import control.ui
	var dragOperation: DragOperation = null

	override final def start(op: DragOperation) {
		ui.dragState match {
			case m: MouseDown => {
				dragOperation = op
				ui.dragState = Dragging
				start(op, m)
			}
			case _ => error("Cannot drag when not in mouse-down state.")
		}
	}
	def start(op: DragOperation, mouseDown: MouseDown)


	override def stop {
		require(ui.dragState == Dragging)
		ui.dragState = NoDrag
	}

}
