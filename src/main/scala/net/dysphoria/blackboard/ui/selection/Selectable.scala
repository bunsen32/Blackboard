/*
 * Selectable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.selection

abstract class Selectable {
	def contains(other: Selectable) = (this == other)
	def orElse(other: => Selectable) = this
}

case object NullSelection extends Selectable {
	override def orElse(other: => Selectable) = other
}