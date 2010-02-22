/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

sealed abstract class CompassPosition {
	val orientation: Orientation
	val end: End
	val forwardBack: Direction
	def opposite = CompassPosition(orientation, end.other)
}

object Up extends CompassPosition {
	val orientation = Vertical
	val end = First
	val forwardBack = Back
}
object Down extends CompassPosition {
	val orientation = Vertical
	val end = Last
	val forwardBack = Forward
}
object Left extends CompassPosition {
	val orientation = Horizontal
	val end = First
	val forwardBack = Back
}
object Right extends CompassPosition {
	val orientation = Horizontal
	val end = Last
	val forwardBack = Forward
}


object CompassPosition {
	def apply(o: Orientation, e: End) =
		if (o == Vertical)
			if (e == First) Up else Down
		else
			if (e == First) Left else Right

	def apply(o: Orientation, d: Direction) =
		if (o == Vertical)
			if (d == Back) Up else Down
		else
			if (d == Forward) Left else Right
}