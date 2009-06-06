package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._
import DisplayDimension._

abstract class DisplayBlock extends Displayable{
	private var _isOwned = false
	private var _xDimensions: List[DisplayDimension] = _
	private var _yDimensions: List[DisplayDimension] = _

	def isOwned = _isOwned
	def xDimensions = _xDimensions
	def yDimensions = _yDimensions

	def disown {
		require(isOwned)

		_xDimensions = Nil
		_yDimensions = Nil
		_isOwned = false
	}

	def own(xs: List[DisplayDimension], ys: List[DisplayDimension]){
		require(!isOwned)
		_xDimensions = xs
		_yDimensions = ys
		_isOwned = true
	}

	def size = new Point(widthOf(xDimensions), widthOf(yDimensions))
}

