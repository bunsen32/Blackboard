package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._
import DisplayDimension._

abstract class DisplayBlock extends Displayable {
	private var _owner: Option[MetaGrid] = None
	private var _xIndex = 0
	private var _yIndex = 0
	private var _xDimensions: List[DisplayDimension] = Nil
	private var _yDimensions: List[DisplayDimension] = Nil

	def hasPosition = _owner.isDefined
	def assertHasPosition = if (!hasPosition) throw new IllegalStateException
	def xIndex = {
		assertHasPosition
		_xIndex
	}
	def yIndex = {
		assertHasPosition
		_yIndex
	}
	def xDimensions = {
		assertHasPosition
		_xDimensions
	}
	def yDimensions = {
		assertHasPosition
		_yDimensions
	}

	def resetPosition {
		_owner = None
	}

	def stripIndex(implicit o: Orientation) = o.choose(xIndex, yIndex)
	def stripDepth(implicit o: Orientation) = o.choose(yIndex, xIndex)

	def setPosition(parent: MetaGrid, x: Int, y: Int){
		_owner = Some(parent)
		_xIndex = x
		_yIndex = y
		_xDimensions = parent.xDimensionLists(x)
		_yDimensions = parent.yDimensionLists(y)
	}

	def size = new Point(widthOf(xDimensions), widthOf(yDimensions))
}

