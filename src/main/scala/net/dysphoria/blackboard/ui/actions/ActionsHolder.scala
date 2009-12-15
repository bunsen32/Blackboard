/*
 * ActionsHolder.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.actions

trait ActionsHolder {
	def app: Application
	def currentView = app.currentView
	def currentSelection = app.currentSelection
}
