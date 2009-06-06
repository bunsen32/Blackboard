package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._
import DisplayDimension._
import scala.collection.mutable._
import selection._

/**
 * A particular layout of tables and dimension labels. Each row and each column has zero or more 
 * (display) dimensions associated with it. Each cell in the MetaGrid may be itself be a table grid.
 */
class MetaGrid extends Displayable {
	private var _xGridSize = 0
	private var _yGridSize = 0
	private var _blocks: Array[Array[Option[DisplayBlock]]] = _
	val xDimensionLists = new DimensionListVector
	val yDimensionLists = new DimensionListVector
	var selection: Selectable = NullSelection

	class DimensionListVector extends RandomAccessSeq[List[DisplayDimension]] {
		private var array = new Array[List[DisplayDimension]](4)
		private var _size = 0

		override def length = _size
		override def size = _size
		private[MetaGrid] def size_=(newSize: Int) {
			if (newSize != size){
				if (newSize < size)
					for(i <- newSize to size)
						array(i) = Nil
				
				_size = newSize
			}
		}

		override def apply(pos: Int) = {
			require(pos >= 0 && pos < size)
			if (pos >= array.size)
				Nil
			else
				array(pos) match {case null => Nil; case list => list}
		}

		def update(pos: Int, v: List[DisplayDimension]){
			require(pos >= 0 && pos < size)
			if (v != this(pos)){
				if (pos >= array.size) ensureSize(size)
				array(pos) = v
			}
		}

		private def ensureSize(sz: Int){
			if (sz > array.length){
				val newArray = new Array[List[DisplayDimension]](sz + array.length)
				Array.copy(array, 0, newArray, 0, array.length)
				array = newArray
			}
		}
		
		def hitTest(p: Int): Option[(Int, Int)] = {
			var i = 0
			var x = 0
			if (p < 0) return None
			while(i < length){
				val limitX = x + widthOf(this(i))
				if (p < limitX) return Some((i, p - x))
				i += 1
				x = limitX
			}
			return None
		}
	}

	def size = new Point(
        (0 /: xDimensionLists)(_ + widthOf(_)),
        (0 /: yDimensionLists)(_ + widthOf(_)))

	def render(gfx: Gfx, position: Point) {
        var y = 0
        var iy = 0
		for(dy <- yDimensionLists){
			var x = 0
            var ix = 0
            for(dx <- xDimensionLists){
				this(ix, iy) match {
					case Some(b) => 
						b.render(gfx, new Point(position.x + x, position.y + y))
						
					case None => /*ignore*/;
                }
				x += widthOf(dx)
                ix += 1
            }
			y += widthOf(dy)
            iy += 1
        }
	}
	
	def hitTest(point: Point): Option[(Int, Int, Point)] = {
		xDimensionLists.hitTest(point.x) match {
			case None => None
			case Some((iX, offsetX)) => {
				yDimensionLists.hitTest(point.y) match {
					case None => None
					case Some((iY, offsetY)) => Some((iX, iY, new Point(offsetX, offsetY)))
				}
			}
		}
	}
	
	
	def setGridSize(newXGridSize: Int, newYGridSize: Int){
		xGridSize = newXGridSize
		yGridSize = newYGridSize
	}

	def xGridSize = _xGridSize
	def xGridSize_=(newXGridSize: Int){
		require(newXGridSize >= 0)
		if (newXGridSize < xGridSize)
			clear(newXGridSize to xGridSize, 0 to yGridSize)
			
		_xGridSize = newXGridSize
		xDimensionLists.size = newXGridSize
	}

	def yGridSize = _yGridSize
	def yGridSize_=(newYGridSize: Int){
		require(newYGridSize >= 0)
		if (newYGridSize < yGridSize)
			clear(0 to xGridSize, newYGridSize to yGridSize)

		_yGridSize = newYGridSize
		yDimensionLists.size = newYGridSize
	}
	
	private def clear(xRange: Range, yRange: Range) {
		for(col <- xRange; row <- yRange) this(row, col) = None
	}

	def apply(x: Int, y: Int) = {
		require( x >= 0 && x < xGridSize && y >= 0 && y < yGridSize)
		if(_blocks == null || x >= _blocks.length)
			None
			
		else{
			val col = _blocks(x)
			if (col == null || y >= col.length)
				None

			else
				col(y) match {
					case null => None
					case cell => cell
				}
		}
	}
	
	def update(x: Int, y: Int, cell: Option[DisplayBlock]){
		val existing = this(x, y)
		if (existing != cell){
			existing match {
				case None => ; // Do nothing
				case Some(block) => block disown
			}
			cell match {
				case None => ; // Do nothing
				case Some(block) => {
					_blocks = ensureLength(_blocks, xGridSize)
					_blocks(x) = ensureLength(_blocks(x), yGridSize)
					block.own(xDimensionLists(x), yDimensionLists(y))
				}
			}
			_blocks(x)(y) = cell
		}
	}

	private def ensureLength[T](original: Array[T], length: Int) = {
		if (original == null || original.length < length){
			val newArray = new Array[T](length)
			if (original != null)
				Array.copy(original, 0, newArray, 0, original.length)
			newArray
		}else
			original
	}
}
