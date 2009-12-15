/*
 * EventStateMask.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Event

/**
 * Interpretation of platform mouse buttons/ modifier keys. Potentially different
 * implementations of this class are required per platform.
 */
class EventState(val e: Event) {
	import e._

	// According to http://book.javanb.com/swt-the-standard-widget-toolkit/ch02lev1sec2.html
	// Primary modifier (on Windows, Ctrl; on Macintosh, Cmd)
	def isMultiSelect = (stateMask & SWT.MOD1) != 0
	// Secondary modifier (Shift)
	def isExtendSelect = (stateMask & SWT.MOD2) != 0
	// Tertiary modifier (Alt/Option)
	def isAltBehaviour = (stateMask & SWT.MOD3) != 0
	// Primary mouse button (for right-handed people: left-mouse-button)
	def isPrimaryButton = (stateMask & SWT.BUTTON1) != 0 || button == 1
	// Secondary mouse button (typically right-mouse-button; will probably be used to invoke a context menu.)
	def isSecondaryButton = (stateMask & SWT.BUTTON2) != 0 || button == 2
}
