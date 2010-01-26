/*
 * Application.scala
 *
 * Part of
 */

package net.dysphoria.blackboard

import ui.ViewCanvas
import ui.actions._

abstract class Application {
	object actions extends ActionsHolder with TableActions {

		override def app = Application.this
	}

	def currentView: ViewCanvas
	def currentSelection = currentView.ui.selection
}
