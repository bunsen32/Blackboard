/*
 * Action.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.actions

import org.eclipse.swt.SWT

abstract class Action {

	def name: String
	def shortName = name
	def accelerator = SWT.NONE
	def isApplicable: Boolean
	final def apply(){
		if (!isApplicable) throw new InapplicableActionException
		safeApply()
	}
	protected def safeApply()
}
