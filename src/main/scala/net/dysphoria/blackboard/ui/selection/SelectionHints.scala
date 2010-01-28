package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard.ui.{Orientation, Axis}

/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 25-Jan-2010
 * Time: 17:21:27
 * To change this template use File | Settings | File Templates.
 */

abstract class SelectionHints(val coords: Map[Axis, Int]) {
	def forAxes(axes: Iterable[Axis]) =
		for(axis <- axes) yield (axis, coords.getOrElse(axis, 0))

	def forDirection(newOrientation: Orientation, previousSelection: Selectable): SelectionHints

	protected def safeForDirection(newOrientation: Orientation, previousSelection: Selectable) = previousSelection match {
		case dataSelection: DataSelection  =>
			val newCoords = if (coords.isEmpty) dataSelection.unambiguousCoords else coords ++ dataSelection.unambiguousCoords
			new ConcreteSelectionHints(newOrientation, newCoords)
		case _ => this
	}

}

class ConcreteSelectionHints(orientation: Orientation, coords: Map[Axis, Int]) extends SelectionHints(coords){
	def forDirection(newOrientation: Orientation, previousSelection: Selectable) =
		if (newOrientation == orientation)
			safeForDirection(newOrientation, previousSelection)
		else
			NullSelectionHints.forDirection(newOrientation, previousSelection)
}

object NullSelectionHints extends SelectionHints(Map.empty) {
	def forDirection(newOrientation: Orientation, previousSelection: Selectable) = safeForDirection(newOrientation, previousSelection)
}