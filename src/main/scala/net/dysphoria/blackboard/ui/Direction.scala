/*
 * Direction.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

sealed abstract class Direction {
	def delta: Int
	def isForward: Boolean
	def isBack: Boolean
}

object Forward extends Direction {
	def delta = +1
	def isForward = true
	def isBack = false
}

object Back extends Direction {
	def delta = -1
	def isForward = false
	def isBack = true
}

