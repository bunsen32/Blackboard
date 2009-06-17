package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.data._
import blackboard.gfx._

class DimensionLabelsBlock(val displayDimension: DisplayDimension) 
		extends TableBlock(displayDimension.dim) {

	dimensionMap = Map(displayDimension -> 0)
	override def requireValidDimensionMap{}

	/**
	 * Returns whether this block displays one of the X dimensions (as opposed to
	 * one of the Y dimensions). If true, this is a horizontal list of labels;
	 * if false, this is a vertical list of labels.
	 */
	def isXNotY = orientation.isX
	def orientation = {
		val isInX = xDimensions contains displayDimension
		val isInY = yDimensions contains displayDimension
		require(isInX || isInY)
		assert(! (isInX && isInY))
		if (isInX) XOrientation else YOrientation
	}

	// More efficient implementation of lookup for one-dimensional block.
	override def displayToTableCoordinates(xCoords: Seq[Int], yCoords: Seq[Int]): Seq[Int] =
		List(displayToTableCoordinate(xCoords, yCoords))

	def displayToTableCoordinate(xCoords: Seq[Int], yCoords: Seq[Int]): Int = {
		if (isXNotY)
			xCoords(xDimensions indexOf displayDimension)
		else
			yCoords(yDimensions indexOf displayDimension)
	}

}

