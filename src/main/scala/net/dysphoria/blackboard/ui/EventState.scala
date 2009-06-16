/*
 * EventStateMask.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT._

class EventState(stateMask: Int) {

	// According to http://book.javanb.com/swt-the-standard-widget-toolkit/ch02lev1sec2.html
	// Primary modifier (on Windows, Ctrl; on Macintosh, Cmd)
	def isMultiSelect = (stateMask & MOD1) != 0
	// Secondary modifier (Shift)
	def isExtendSelect = (stateMask & MOD2) != 0
	// Tertiary modifier (Alt/Option)
	def isAltBehaviour = (stateMask & MOD3) != 0
	// Primary mouse button (for right-handed people: left-mouse-button)
	def isPrimaryButton = (stateMask & BUTTON1) != 0
}
