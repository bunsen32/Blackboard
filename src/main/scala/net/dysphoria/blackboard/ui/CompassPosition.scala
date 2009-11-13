/*
 * CompassPosition.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

sealed abstract class CompassPosition {
	val orientation: Orientation
	val end: End
}

object Up extends CompassPosition {
	val orientation = YOrientation
	val end = First
}
object Down extends CompassPosition {
	val orientation = YOrientation
	val end = Last
}
object Left extends CompassPosition {
	val orientation = XOrientation
	val end = First
}
object Right extends CompassPosition {
	val orientation = XOrientation
	val end = Last
}


object CompassPosition {
	def apply(o: Orientation, e: End) =
		if (o == YOrientation)
			if (e == First) Up else Down
		else
			if (e == First) Left else Right

}