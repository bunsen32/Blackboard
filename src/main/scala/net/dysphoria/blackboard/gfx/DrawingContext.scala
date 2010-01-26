/*
 * Gfx.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.gfx

import scala.collection._
import org.eclipse.swt.graphics._
import net.dysphoria.blackboard.ui._

class DrawingContext(val gc: GC, val ui: UIState) {

	private val resourceMap = new mutable.HashMap[Any, Resource]
	private val resources = mutable.Set.empty[Resource]
	
	// If coordinates aren’t aligned with screen pixels, how much do I have to add to a coordinate to
	// cover the next pixel?
	protected val pixelDistance = 1

	def colorForRGB(col: RGB) =
		resourceMap.getOrElseUpdate(col, register(new Color(gc.getDevice, col))).asInstanceOf[Color]

	def font(family: String, size: Float, style: Int) = {
		val key = (family, size.toInt, style)
		resourceMap.getOrElseUpdate(
			key,
			register(new Font(gc.getDevice,
							  family, size.toInt, style))).asInstanceOf[Font]
	}

	def newTransform = register(new Transform(gc.getDevice))

	def register[R <: Resource](res: R) = {
		resources += res
		res
	}

	def dispose {
		for(r <- resources)
			r.dispose

		resources clear
	}

	private var currentClipRect: Rectangle = null

	/**
	 * Execute the given ‘op’ while the graphics context is clipped to the given
	 * cliprect. Note that changing the transform screws up nested clipping completely,
	 * so only call this in a context where the graphics transformation stays stable.
	 */
	def withclip(rect: Rectangle)(op: => Unit) {
		val oldRect = if (currentClipRect == null) gc.getClipping else currentClipRect
		var newRect = (rect intersection oldRect)
		if (! newRect.isEmpty) {
			currentClipRect = newRect
			gc.setClipping(currentClipRect)
			try {
				op

			}finally{
				currentClipRect = oldRect
				gc.setClipping(currentClipRect)
			}
		}
	}

	def roundDown(c: Int) = c - pixelDistance
	def roundUp(c: Int) = c + pixelDistance

	def setLine(l: LineDescriptor){
		gc setLineAttributes l.lineAttributes
		gc setForeground colorForRGB(l.colour)
	}



	/* Tried a region-based solution, but it a) failed to deal with problems
	 * I was having, re: inaccuracy when clipping within a transformed gc, and
	 * b) appeared to be much slower.

	private val clipRegions = new mutable.ArrayBuffer[Region]{
		override def apply(ix: Int) = {
			if (ix == length){
				val r = register(new Region(gc.getDevice))
				append(r)
				r
				
			}else
				super.apply(ix)
		}
	}
	private var clipNest = 0
	def withclip(rect: Rectangle)(op: => Unit) {
		val oldClip = clipRegions(clipNest)
		gc.getClipping(oldClip)
		try {
			clipNest += 1
			val newClip = clipRegions(clipNest)
			gc.getClipping(newClip)
			newClip.intersect(rect)
			gc.setClipping(newClip)
			op
			
		}finally{
			clipNest -= 1
			gc.setClipping(oldClip)
		}
	}
	*/
}
