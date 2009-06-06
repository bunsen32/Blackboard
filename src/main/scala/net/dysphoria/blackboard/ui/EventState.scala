/*
 * EventStateMask.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT._

class EventState(stateMask: Int) {

	def isMultiSelect = (stateMask & COMMAND) != 0
	def isExtendSelect = (stateMask & SHIFT) != 0
	def isAltBehaviour = (stateMask & ALT) != 0
	def isPrimaryButton = (stateMask & BUTTON1) != 0
}
