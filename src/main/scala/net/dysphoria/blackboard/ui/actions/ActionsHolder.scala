/*
 * ActionsHolder.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.actions

import net.dysphoria.blackboard.Application

trait ActionsHolder {
	def app: Application
	def currentView = app.currentView
	def currentSelection = app.currentSelection
}
