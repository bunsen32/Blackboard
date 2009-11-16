/*
 * LabelSelection.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

/**
 * A single selected axis label. Includes
 */
case class LabelSelection(block: TableBlock, orientation: Orientation, coords: Map[Axis,Int]) extends SingleGridSelection {
	val actualB = 0
	lazy val parentCoords = coords -- (block.axes(orientation))
	lazy val axisIndex = {
		val num =
			block.axes(orientation)
			.takeWhile(coords.contains(_))
			.length
		assert(num != 0)
		num - 1
	}
	def axis = block.axes(orientation)(axisIndex)
	def index = coords(axis)

	def to(other: LabelSelection): Selectable = {
		if (this.block == other.block &&
			this.orientation == other.orientation &&
			this.parentCoords == other.parentCoords){
			
			// This method may be too clever for its own good (or at least the user’s
			// comprehension). Not convinced that we should be returning anything
			// if ‘this’ and ‘other’ are at different levels.
			val availableAxes = block.axes(orientation)
			val commonAxes = availableAxes
				.takeWhile(ax => (this.coords.get(ax) == other.coords.get(ax)))
			val matchLength = commonAxes.length

			if (matchLength == availableAxes.length)
				this

			else {
				assert (matchLength < availableAxes.length)
				val commonCoords = commonAxes.map(ax => (ax, coords.getOrElse(ax, 0)))
				val ax = availableAxes(matchLength) // next axis
				(this.coords.get(ax), other.coords.get(ax)) match {
					case (Some(thisV), Some(otherV)) =>
						assert(thisV != otherV)
						val r = if (thisV < otherV) thisV to otherV else otherV to thisV
						LabelRangeSelection(block, orientation, parentCoords ++ commonCoords, ax, r)

					case _ =>
						LabelSelection(block, orientation, parentCoords ++ commonCoords)
				}
			}
			
		}else
			NullSelection
	}
	
}
