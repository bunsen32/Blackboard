/*
 * Gfx.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.gfx

import scala.collection._
import org.eclipse.swt.graphics._
import blackboard.ui._

class DrawingContext(val gc: GC, val ui: UIState) {

	private val resourceMap = new mutable.HashMap[Any, Resource]
	private val resources = mutable.Set.empty[Resource]

	def colorForRGB(col: RGB) =
		resourceMap.getOrElseUpdate(col, register(new Color(gc.getDevice, col))).asInstanceOf[Color]

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

	def withclip(rect: Rectangle)(op: => Unit) {
		val oldClip = gc.getClipping()
		try {
			gc.setClipping(rect intersection oldClip)
			op

		}finally{
			gc.setClipping(oldClip)
		}
	}
	/* Tried a region-based solution, but it a) failed to deal with problems
	 * I was having, re: innaccuracy when clipping within a tranformed gc, and
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
