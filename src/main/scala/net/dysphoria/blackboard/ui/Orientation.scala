/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.{Point, Rectangle}

sealed abstract class Orientation {
	def isX: Boolean
	def isY: Boolean
	def other: Orientation

	def choose(x: Int, y: Int): Int
	@inline
	def choose[A](x: =>A, y: =>A): A

	def orient(a: Int, b: Int): (Int, Int)

	def newPoint(breadth: Int, depth: Int): Point
	def breadth(p: Point): Int
	def depth(p: Point): Int
	def breadthDepth(p: Point): (Int, Int)

	def newRectangle(b0: Int, d0: Int, b: Int, d: Int): Rectangle
}

case object Horizontal extends Orientation {
	def isX = true
	def isY = false
	def other = Vertical

	def choose(x: Int, y: Int) = x
	def choose[A](x: =>A, y: =>A) = x

	def orient(x: Int, y: Int) = (x, y)

	def newPoint(breadth: Int, depth: Int) = new Point(breadth, depth)
	def breadth(p: Point) = p.x
	def depth(p: Point) = p.y
	def breadthDepth(p: Point) = (p.x, p.y)

	def newRectangle(b0: Int, d0: Int, b: Int, d: Int) =
		new Rectangle(b0, d0, b, d)
}

case object Vertical extends Orientation {
	def isX = false
	def isY = true
	def other = Horizontal

	def choose(x: Int, y: Int) = y
	def choose[A](x: =>A, y: =>A) = y

	def orient(y: Int, x: Int) = (y, x)

	def newPoint(breadth: Int, depth: Int) = new Point(depth, breadth)
	def breadth(p: Point) = p.y
	def depth(p: Point) = p.x
	def breadthDepth(p: Point) = (p.y, p.x)

	def newRectangle(b0: Int, d0: Int, b: Int, d: Int) =
		new Rectangle(d0, b0, d, b)
}
