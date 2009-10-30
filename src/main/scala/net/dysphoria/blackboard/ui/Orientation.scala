/*
 * Orientation.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Point

sealed abstract class Orientation {
	def isX: Boolean
	def isY: Boolean
	def opposite: Orientation

	def choose(x: Int, y: Int): Int
	@inline
	def choose[A](x: =>A, y: =>A): A

	def orient(a: Int, b: Int): (Int, Int)

	def newPoint(breadth: Int, depth: Int): Point
	def breadth(p: Point): Int
	def depth(p: Point): Int
	def breadthDepth(p: Point): (Int, Int)
}

case object XOrientation extends Orientation {
	def isX = true
	def isY = false
	def opposite = YOrientation

	def choose(x: Int, y: Int) = x
	def choose[A](x: =>A, y: =>A) = x

	def orient(x: Int, y: Int) = (x, y)

	def newPoint(breadth: Int, depth: Int) = new Point(breadth, depth)
	def breadth(p: Point) = p.x
	def depth(p: Point) = p.y
	def breadthDepth(p: Point) = (p.x, p.y)
}

case object YOrientation extends Orientation {
	def isX = false
	def isY = true
	def opposite = XOrientation

	def choose(x: Int, y: Int) = y
	def choose[A](x: =>A, y: =>A) = y

	def orient(y: Int, x: Int) = (y, x)

	def newPoint(breadth: Int, depth: Int) = new Point(depth, breadth)
	def breadth(p: Point) = p.y
	def depth(p: Point) = p.x
	def breadthDepth(p: Point) = (p.y, p.x)
}
