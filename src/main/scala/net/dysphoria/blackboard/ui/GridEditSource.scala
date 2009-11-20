/*
 * GridEditSource.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Rectangle

abstract class GridEditSource {
	def read: String
	def write(s: String)
	def getBounds(canvas: ViewCanvas): Rectangle
}
