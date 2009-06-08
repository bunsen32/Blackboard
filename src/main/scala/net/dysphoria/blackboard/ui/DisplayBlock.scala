package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._
import DisplayDimension._

abstract class DisplayBlock extends Displayable {
	private var _owner: Option[MetaGrid] = None
	private var _xIndex = 0
	private var _yIndex = 0
	private var _xDimensions: List[DisplayDimension] = _
	private var _yDimensions: List[DisplayDimension] = _

	def isOwned = _owner.isDefined
	def xIndex = _xIndex
	def yIndex = _yIndex
	def xDimensions = _xDimensions
	def yDimensions = _yDimensions

	def disown {
		require(isOwned)

		_xDimensions = Nil
		_yDimensions = Nil
		_owner = None
	}

	def own(parent: MetaGrid, x: Int, y: Int){
		require(!isOwned)
		_owner = Some(parent)
		_xIndex = x
		_yIndex = y
		_xDimensions = parent.xDimensionLists(x)
		_yDimensions = parent.yDimensionLists(y)
	}

	def size = new Point(widthOf(xDimensions), widthOf(yDimensions))
}

