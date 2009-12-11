/*
 * End.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

sealed abstract class End {
	def isFirst: Boolean
	def isLast: Boolean
	def of[A](s: Seq[A]): A
}

case object First extends End {
	def isFirst = true
	def isLast = false
	def of[A](s: Seq[A]) = s.first
}

case object Last extends End {
	def isFirst = false
	def isLast = true
	def of[A](s: Seq[A]) = s.last
}