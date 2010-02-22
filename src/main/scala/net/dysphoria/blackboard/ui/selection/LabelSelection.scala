/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.model._

abstract class LabelSelection extends TableSubItemSelection {
	def container: LabelContainer
	def ownerPartModel: TablePart
	def containingPart: TablePart#TablePartInstance

	def orientation: Orientation
	def isLongitudinal(o: Orientation) = o == this.orientation
	def isTransverse(o: Orientation) = o != this.orientation
	def longitudinal = orientation
	def transverse = orientation.other
	def axis: Axis
	def axisIndex: Int

	def table = containingPart.table
	def isDeepest = ownerPartModel.axes(orientation).lengthCompare(axisIndex + 1) match {
		case x if x > 0 => false
		case 0 => true
		case _ => error("Label.axisIndex > ownerPartModel.axes.length")
	}
	def isShallowest = (axisIndex == 0)
}

trait NestedLabelSelection { self: LabelSelection =>
	val containingLabel: LabelInstance
	final def container = containingLabel
	val ownerPartModel: TablePart
	final def containingPart = containingLabel.containingPart
}

trait UnnestedLabelSelection { self: LabelSelection =>
	final def container = containingPart
	final def ownerPartModel = containingPart.model
	val containingPart: TablePart#TablePartInstance
}
