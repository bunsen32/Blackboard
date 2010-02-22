/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

sealed abstract class End {
	def isFirst: Boolean
	def isLast: Boolean
	def of[A](s: Seq[A]): A
	def other: End
}

case object First extends End {
	def isFirst = true
	def isLast = false
	def of[A](s: Seq[A]) = s.first
	def other = Last
}

case object Last extends End {
	def isFirst = false
	def isLast = true
	def of[A](s: Seq[A]) = s.last
	def other = First
}