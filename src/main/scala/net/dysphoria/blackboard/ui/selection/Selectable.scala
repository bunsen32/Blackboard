/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

abstract class Selectable {
	def contains(other: Selectable) = (this == other)
	def orElse(other: => Selectable) = this
}

case object NullSelection extends Selectable {
	override def orElse(other: => Selectable) = other
}