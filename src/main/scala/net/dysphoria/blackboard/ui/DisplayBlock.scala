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

	def hasOrientation = _owner.isDefined
	def assertHasOrientation = if (!hasOrientation) throw new IllegalStateException
	def xIndex = {
		assertHasOrientation
		_xIndex
	}
	def yIndex = {
		assertHasOrientation
		_yIndex
	}
	def xDimensions = {
		assertHasOrientation
		_xDimensions
	}
	def yDimensions = {
		assertHasOrientation
		_yDimensions
	}

	def resetOrientation {
		_owner = None
	}

	def setOrientation(parent: MetaGrid, x: Int, y: Int){
		_owner = Some(parent)
		_xIndex = x
		_yIndex = y
		_xDimensions = parent.xDimensionLists(x)
		_yDimensions = parent.yDimensionLists(y)
	}

	def size = new Point(widthOf(xDimensions), widthOf(yDimensions))
}

