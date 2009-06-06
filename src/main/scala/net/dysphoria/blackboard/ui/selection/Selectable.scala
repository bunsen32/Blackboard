/*
 * Selectable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.selection

abstract class Selectable {
	def contains(other: Selectable) = false
}

case object NullSelection extends Selectable