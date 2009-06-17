/*
 * Orientation.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

sealed abstract class Orientation {
	def isX: Boolean
	def isY: Boolean
	def opposite: Orientation

	def choose(x: Int, y: Int): Int
	def choose[A](x: A, y: A): A
}
case object XOrientation extends Orientation {
	def isX = true
	def isY = false
	def opposite = YOrientation

	def choose(x: Int, y: Int) = x
	def choose[A](x: A, y: A) = x
}
case object YOrientation extends Orientation {
	def isX = false
	def isY = true
	def opposite = XOrientation

	def choose(x: Int, y: Int) = y
	def choose[A](x: A, y: A) = y
}
