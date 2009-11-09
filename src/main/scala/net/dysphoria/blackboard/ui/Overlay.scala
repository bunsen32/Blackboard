/*
 * Overlay.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import org.eclipse.swt.widgets._

abstract class Overlay(parent: Shell) {
	val shell = new Shell(parent, SWT.MODELESS | SWT.NO_TRIM)

	def visible = shell.getVisible
	def visible_=(v: Boolean) {
		shell.setVisible(v)
	}

	def setVisible(v: Boolean) {shell.setVisible(v)}

	def location = shell.getLocation
	def location_=(p: Point) {
		shell.setLocation(p)
	}
	def setLocation(x: Int, y: Int) {
		shell.setLocation(x, y)
	}
}
