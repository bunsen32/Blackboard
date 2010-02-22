/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
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
