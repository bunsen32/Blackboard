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
	private var _positionsCurrent = false
	val xDimensionLists = new DimensionListVector
	val yDimensionLists = new DimensionListVector

	class DimensionListVector extends RandomAccessSeq[List[DisplayDimension]] {
		private var array = new Array[List[DisplayDimension]](4)
		private var _size = 0

		override def length = _size
		override def size = _size
		private[MetaGrid] def size_=(newSize: Int) {
			if (newSize != size){
				if (newSize < size)
					for(i <- newSize until size)
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
		
		/**
		 * Given a distance coordinate in pixels, <var>p</var>, returns the 'gap'
		 * (between DimensionList indices) that it's nearest to and the pixel 
		 * offset from that gap. Zero is the gap before the first DimensionList;
		 * one is the gap between the first and second DimensionList, and so on.
		 * If there are n DimensionLists, 'n' is the gap after the last one.
		 */
		def gapHitTest(p: Int): (Int, Int) = {
			var i = 0
			var x = 0
			while(i < length){
				val w = widthOf(this(i))
				if (p < x + (w/2)) return (i, p-x)
				i += 1
				x += w
			}
			return (length, p-x)
		}

		def gapPosition(i: Int) = {
			require(i >= 0 && i <= length)
			(0 /: this.take(i))(_ + widthOf(_))
		}
		
		// MUTATORS
		
		def update(pos: Int, v: List[DisplayDimension]){
			require(pos >= 0 && pos < size)
			if (v != this(pos)){
				if (pos >= array.size) ensureSize(size)
				array(pos) = v
				dirtyLayout
			}
		}

		private[MetaGrid] def insert(pos: Int){
			require(pos >= 0 && pos <= size)
			val oldSize = size
			size = oldSize + 1
			ensureSize(size)
			val next = pos+1
			if (pos < oldSize) Array.copy(array, pos, array, next, oldSize - pos)
			array(pos) = Nil
		}
		
		private[MetaGrid] def delete(pos: Int){
			require(pos >= 0 && pos < size)
			val next = pos+1
			if (next < size) Array.copy(array, next, array, pos, size - next)
			size -= 1
		}
	}

	def size = new Point(
        (0 /: xDimensionLists)(_ + widthOf(_)),
        (0 /: yDimensionLists)(_ + widthOf(_)))

	def render(gfx: DrawingContext, position: Point) {
        var y = position.y
        var iy = 0
		for(dy <- yDimensionLists){
			val height = widthOf(dy)
			var x = position.x
            var ix = 0
            for(dx <- xDimensionLists){
				val width = widthOf(dx)
				val b = this(ix, iy)
				b.render(gfx, new Point(x, y))
				if (gfx.ui.selectLargeBits) {
					import gfx.gc._
					import Style._
					setAlpha(BlockFillAlpha)
					setBackground(gfx.colorForRGB(BlockFill))
					fillRoundRectangle(x, y, width, height, 3, 3)
					setAlpha(255)
					BlockOutline.setAttributesOf(gfx)
					drawRoundRectangle(x, y, width, height, 3, 3)
				}

				gfx.ui.selection match {
					case SingleGridSpace(_, jx, jy) if (jx==ix && jy==iy) => {
						import gfx.gc._
						import Style._
						setAlpha(BlockFillAlpha)
						setBackground(gfx.colorForRGB(BlockFill))
						fillRoundRectangle(x, y, width, height, 3, 3)
						setAlpha(255)
						BlockOutline.setAttributesOf(gfx)
						drawRoundRectangle(x, y, width, height, 3, 3)
					}
					case _ => ;//ignore
				}

				x += width
                ix += 1
            }
			y += height
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
			clear(newXGridSize until xGridSize, 0 until yGridSize)
			
		_xGridSize = newXGridSize
		xDimensionLists.size = newXGridSize
	}

	def yGridSize = _yGridSize
	def yGridSize_=(newYGridSize: Int){
		require(newYGridSize >= 0)
		if (newYGridSize < yGridSize)
			clear(0 until xGridSize, newYGridSize until yGridSize)

		_yGridSize = newYGridSize
		yDimensionLists.size = newYGridSize
	}
	
	private def clear(xRange: Range, yRange: Range) {
		for(col <- xRange; row <- yRange) this(col, row) = None
	}

	def apply(x: Int, y: Int) = {
		ensurePositions
		val result =
			optionBlockAt(x, y) match {
				case Some(b) => b
				case None => {
					val newB = new EmptyBlock
					update(x, y, Some(newB))
					newB
				}
			}
		if (!result.hasPosition) result.setPosition(this, x, y)
		result
	}
	
	def update(x: Int, y: Int, cell: Option[DisplayBlock]){
		val existing = optionBlockAt(x, y)
		if (existing != cell){
			existing match {
				case None => ; // Do nothing
				case Some(block) => block.resetPosition
			}
			cell match {
				case None => ; // Do nothing
				case Some(block) => {
					block.resetPosition
					_blocks = ensureLength(_blocks, xGridSize)
					_blocks(x) = ensureLength(_blocks(x), yGridSize)
				}
			}
			_blocks(x)(y) = cell
		}
	}

	def optionBlockAt(x: Int, y: Int) = {
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

	private def ensureLength[T](original: Array[T], length: Int) = {
		if (original == null || original.length < length){
			val newArray = new Array[T](length)
			if (original != null)
				Array.copy(original, 0, newArray, 0, original.length)
			newArray
		}else
			original
	}


	/**
	 * Remove any entirely empty rows or columns
	 */
	def compress {
		for(y <- (0 until yGridSize).reverse)
			if (row(y) forall (_.isInstanceOf[EmptyBlock]))
				deleteRow(y)
		for(x <- (0 until xGridSize).reverse)
			if (column(x) forall (_.isInstanceOf[EmptyBlock]))
				deleteCol(x)
	}


	def strip(o: Orientation, j: Int) =
		if (o.isX) column(j) else row(j)

	def column(ix: Int): Seq.Projection[DisplayBlock] =
		for(y <- 0 until yGridSize)
			yield this(ix, y)

	def row(iy: Int): Seq.Projection[DisplayBlock] =
		for(x <- 0 until xGridSize)
			yield this(x, iy)


	def insertStrip(o: Orientation, j: Int) =
		if (o.isX) insertCol(j) else insertRow(j)

	def insertCol(x: Int){
		require(x >= 0 && x <= xGridSize)
		dirtyLayout
		_blocks = insertInto(_blocks, x, _xGridSize)
		_xGridSize += 1
		xDimensionLists.insert(x)
	}
	
	def insertRow(y: Int){
		require(y >= 0 && y <= yGridSize)
		dirtyLayout
		for(x <- 0 until _blocks.length)
			if (_blocks(x) != null)
				_blocks(x) = insertInto(_blocks(x), y, _yGridSize)
		_yGridSize += 1
		yDimensionLists.insert(y)
	}


	def deleteStrip(o: Orientation, j: Int) =
		if (o.isX) deleteCol(j) else deleteRow(j)

	def deleteCol(x: Int){
		require(x >= 0 && x < xGridSize)
		dirtyLayout
		_blocks = deleteFrom(_blocks, x, _xGridSize)
		_xGridSize -= 1
		xDimensionLists.delete(x)
	}
	
	def deleteRow(y: Int){
		require(y >=0 && y < yGridSize)
		dirtyLayout
		for(x <- 0 until _blocks.length)
			if (_blocks(x) != null)
				_blocks(x) = deleteFrom(_blocks(x), y, _yGridSize)
		_yGridSize -= 1 // do this after the 'deleteFrom'
		yDimensionLists.delete(y)
	}


/*	private def ensureOrientations {
		if (!_orientationsCurrent) {
			for(x <- 0 until xGridSize; y <- 0 until yGridSize)
				apply(x, y).setOrientation(this, x, y)
			_orientationsCurrent = true
		}
	}
*/

	private def dirtyLayout {
		_positionsCurrent = false
	}

	private def ensurePositions {
		if (!_positionsCurrent){
			for(col <- _blocks; if col != null; b <- col) b match {
				case Some(block) => block.resetPosition
				case _ => ;//ignore
			}
			_positionsCurrent = true
		}
	}



	private def insertInto[T >: Null](original: Array[T], pos: Int, originalRealLength: Int) = {
		require(pos >= 0 && pos <= originalRealLength)
		if (original == null || pos >= original.length)
			original
		else {
			val newRealLength = originalRealLength + 1
			val result = if (original.length >= newRealLength)
					original
				else {
					val newArray = new Array[T](newRealLength)
					assert(pos < original.length)
					Array.copy(original, 0, newArray, 0, pos)
					newArray
				}
			val next = pos + 1
			val end = (original.length min originalRealLength)
			Array.copy(original, pos, result, next, end - pos)
			original(pos) = null
			result
		}
	}

	private def deleteFrom[T >: Null](original: Array[T], pos: Int, originalRealLength: Int) = {
		require(pos >= 0 && pos < originalRealLength)
		if (original == null || pos >= original.length)
			original

		else {
			val newRealLength = originalRealLength - 1
			val next = pos + 1
			val end = (original.length min originalRealLength)
			Array.copy(original, next, original, pos, end - next)
			original(end - 1) = null
			original
		}
	}

	def reorderDisplayDimensions(blocks: Seq[DisplayBlock]): List[DisplayDimension] = {
		val result = new ListBuffer[DisplayDimension]
		for(b <- blocks) b match {
			case labs: DimensionLabelsBlock => result.append(labs.displayDimension)
			case _ => ;//ignore
		}
		result.toList
	}

	abstract class Strips {
		def extent: Int
		def depth: Int
		def dimensionLists: DimensionListVector
		def apply(j: Int, k: Int): DisplayBlock
		def update(j: Int, k: Int, b: Option[DisplayBlock])
		def apply(j: Int): Iterable[DisplayBlock]
		def insert(j: Int)
		def remove(j: Int)
	}

	def strips(implicit o: Orientation) = o.choose(columns, rows)

	object columns extends Strips {
		def extent = xGridSize
		def depth = yGridSize
		def dimensionLists = xDimensionLists
		def apply(x: Int, y: Int) = MetaGrid.this(x, y)
		def update(x: Int, y: Int, b: Option[DisplayBlock]) = MetaGrid.this(x, y) = b
		def apply(x: Int) = column(x)
		def insert(x: Int) = insertCol(x)
		def remove(x: Int) = deleteCol(x)
	}

	object rows extends Strips {
		def extent = yGridSize
		def depth = xGridSize
		def dimensionLists = yDimensionLists
		def apply(y: Int, x: Int) = MetaGrid.this(x, y)
		def update(y: Int, x: Int, b: Option[DisplayBlock]) = MetaGrid.this(x, y) = b
		def apply(y: Int) = row(y)
		def insert(y: Int) = insertRow(y)
		def remove(y: Int) = deleteRow(y)
	}

}
