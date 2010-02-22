/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.model._
import org.eclipse.swt.graphics.Point

sealed abstract class LabelRange(val orientation: Orientation, val unambiguousCoords: Map[Axis,Int], val axis: Axis, val range: Range) extends LabelSelection {
	require(range.length > 1, "LabelRange consisting of fewer than 2 labels not allowed")
	//lazy val parentCoords = unambiguousCoords -- (tablePart.axes(orientation))
	lazy val axisIndex =
		ownerPartModel.axes(orientation)
		.takeWhile(unambiguousCoords.contains(_))
		.length

	lazy val first = LabelInstance(container, ownerPartModel, orientation, unambiguousCoords + (axis -> range.first))
	lazy val last = LabelInstance(container, ownerPartModel, orientation, unambiguousCoords + (axis -> range.last))

	override def contains(other: Selectable) = other match {
		case lab: LabelInstance =>
			lab.ownerPartModel == this.ownerPartModel &&
			lab.orientation == this.orientation &&
			lab.axis == this.axis &&
			this.range.contains(lab.index) &&
			contains(lab.coords, this.unambiguousCoords)

		case other: LabelRange => contains(other.first) && contains(other.last)
			
		case _ => false
	}

	private def contains(container: Map[Axis,Int], contained: Map[Axis,Int]) =
		contained.forall(kv => {val (k, v) = kv; container(k) == v})


	def bounds = first.bounds union last.bounds

	def position = {
		val b = first.bounds
		new Point(b.x, b.y)
	}

	override def equals(obj: Any) = obj match {
		case other: LabelRange =>
			this.orientation == other.orientation &&
			this.unambiguousCoords == other.unambiguousCoords &&
			this.axis == other.axis &&
			this.range.first == other.range.first &&
			this.range.last == other.range.last

		case _ => false
	}

	override def hashCode =
		(this.orientation.hashCode * 41) ^
			((this.unambiguousCoords.hashCode * 41) ^
				((this.axis.hashCode * 41) ^
			   		((this.range.first * 41) ^
			   			this.range.end)))
	
}

object LabelRange {
	def apply(parent: LabelContainer, partModel: TablePart, orientation: Orientation, unambiguous: Map[Axis, Int], axis: Axis, range: Range) = parent match {
		case parentLabel: LabelInstance =>
			new LabelRange(orientation, unambiguous, axis, range) with NestedLabelSelection {
				val containingLabel= parentLabel
				val ownerPartModel = partModel
			}
		case parentPart: TablePart#TablePartInstance =>
			assert(partModel == parentPart.model)
			new LabelRange(orientation, unambiguous, axis, range) with UnnestedLabelSelection {
				val containingPart = parentPart
			}
	}
}
