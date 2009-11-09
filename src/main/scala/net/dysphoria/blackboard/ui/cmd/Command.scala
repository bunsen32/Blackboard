/*
 * Command.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.cmd

abstract class Command {
	def apply(cx: Context): Unit
}
