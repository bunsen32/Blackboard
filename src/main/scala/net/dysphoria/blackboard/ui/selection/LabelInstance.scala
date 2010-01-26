/*
  * OneLabel.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.model._

/**
 * A single selected axis label. Includes
 */
sealed abstract class LabelInstance(val orientation: Orientation, val coords: Map[Axis,Int])
		extends LabelSelection with SingleGridSelection with LabelContainer {
			
	//lazy val parentCoords = coords -- (tablePart.axes(orientation))
	lazy val axisIndex = {
		val num =
			ownerPartModel.axes(orientation)
			.takeWhile(coords.contains(_))
			.length
		assert(num != 0)
		num - 1
	}
	def axis = ownerPartModel.axes(orientation)(axisIndex)
	def index = coords(axis)

	def withinData = axis.range contains index

	override def equals(obj: Any) = obj match {
		case other: LabelInstance =>
			this.orientation == other.orientation &&
			this.coords == other.coords
		
		case _ => false
	}

	override def hashCode =
		(this.orientation.hashCode * 41) ^
		(this.coords.hashCode)

	/**
	 * Returns a LabelRangeSelection representing all the labels between ‘this’ and ‘other’.
	 */
	def to(other: LabelInstance): Selectable =
		if (this == other)
			this

		else if (this.table == other.table && this.orientation == other.orientation) {
			val commonContainer = findCommon(this, other)
			val block = commonContainer match {
				case commonLabel: LabelInstance =>
					val owner = commonLabel.ownerPartModel.asInstanceOf[TableStruct]
					owner.elementFor(commonLabel.coords)

				case commonPart: TablePart#TablePartInstance =>
					commonPart.model
			}
			val axes = block.axes(this.orientation)
			assert(!axes.isEmpty)
			@inline def constructLabelRange(coordsSoFar: Map[Axis,Int], axes: Seq[Axis]): LabelSelection = {
				lazy val firstAxis = axes.first
				if (!axes.isEmpty && this.coords.contains(firstAxis) && other.coords.contains(firstAxis)){
					val restOfAxes = axes.drop(1)
					val (a, b) = (this.coords(firstAxis), other.coords(firstAxis))
					if (a == b)
						constructLabelRange(coordsSoFar + ((firstAxis, a)), restOfAxes)
					else {
						val valueRange = (a min b) to (a max b)
						LabelRange(commonContainer, block, this.orientation, coordsSoFar, firstAxis, valueRange)
					}
				}else{
					LabelInstance(commonContainer, block, this.orientation, coordsSoFar)
				}
			}
			constructLabelRange(commonContainer.unambiguousCoords, axes)

		}else
			NullSelection

	def findCommon(a: LabelContainer, b: LabelContainer) = {
		def depthFromTable(item: LabelContainer, soFar: Int): Int = item.container match {
			case t: Table#TableInstance => soFar + 1
			case sub: LabelContainer => depthFromTable(sub, soFar + 1)
			case _ => error("Found thing which is not Table and not TableSubItemInstance!")
		}

		def nthParent(item: LabelContainer, count: Int): LabelContainer = count match {
			case 0 => item
			case n => assert(n > 0); nthParent(item.container.asInstanceOf[LabelContainer], n - 1)
		}

		def common(a: LabelContainer, b: LabelContainer): LabelContainer =
			if (a == b)
				a
			else
				common(a.container.asInstanceOf[LabelContainer], b.container.asInstanceOf[LabelContainer])

		require(a.table == b.table)
		val depthA = depthFromTable(a, 0)
		val depthB = depthFromTable(b, 0)
		val minDepth = depthA min depthB
		val aNormalised = nthParent(a, depthA - minDepth)
		val bNormalised = nthParent(b, depthB - minDepth)
		common(aNormalised, bNormalised)
	}

	def adjacent(direction: CompassPosition)(implicit hint: SelectionHints) = {
		val forwardBack  = direction.forwardBack
		val theAxes = ownerPartModel.axes(orientation)
		if (isLongitudinal(direction.orientation)) {
			val usedAxes = theAxes.take(axisIndex + 1)
			(ownerPartModel.nextOnAxes(usedAxes, coords, forwardBack) map (LabelInstance(container, ownerPartModel, orientation, _))) orElse
				oneBeyondLabelArea(direction)

		}else{ // Moving transversely, orthogonal to axis (between different axes).
			val newAxisIndex = axisIndex + forwardBack.delta
			if (newAxisIndex < 0)
				oneBeyondLabelArea(direction)

			else if (newAxisIndex < theAxes.length) {
				val newCoords =
					if (forwardBack.isForward) {
						val newAxis = theAxes(newAxisIndex)
						val newCoord = hint.coords.getOrElse(newAxis, 0)
						coords + (newAxis -> newCoord)
					}else
						coords - axis
				Some(LabelInstance(container, ownerPartModel, orientation, newCoords))

			}else
				ownerPartModel.firstChildLabelOf(this) orElse
					oneBeyondLabelArea(direction)
		}
	}

	def oneBeyondLabelArea(direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection]

	def oneBeyondChildLabels(direction: CompassPosition)(implicit hint: SelectionHints) = {
		// This method contains the knowledge that our child labels are always the last axes in our label area.
		if (direction.orientation == transverse)
			if (direction.forwardBack.isBack) Some(this) else oneBeyondLabelArea(direction)
		else // Longitudinal
			adjacent(direction)
	}
}

object LabelInstance {
	def apply(parent: LabelContainer, partModel: TablePart, orientation: Orientation, coords: Map[Axis, Int]): LabelInstance = parent match {
		case parentLabel: LabelInstance =>
			new LabelInstance(orientation, coords) with NestedLabelSelection {
				val containingLabel = parentLabel
				val ownerPartModel = partModel
				def oneBeyondLabelArea(direction: CompassPosition)(implicit hint: SelectionHints) = containingLabel.oneBeyondChildLabels(direction)
			}
		case parentPart: TablePart#TablePartInstance =>
			assert(partModel == parentPart.model)
			new LabelInstance(orientation, coords) with UnnestedLabelSelection {
				val containingPart = parentPart
				def oneBeyondLabelArea(direction: CompassPosition)(implicit hint: SelectionHints) = containingPart.container.oneBeyondLabelArea(this, direction)
			}
	}
}
