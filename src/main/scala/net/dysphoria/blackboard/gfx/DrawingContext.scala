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
	private val highlightRGB = new RGB(255, 255, 0)

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

	def renderBasicCell(style: CellStyle, bounds: Rectangle, value: String, selectIntensity: Int, greyed: Boolean) {
		val bg = mixBackground(style.backgroundColor, selectIntensity)
		val fg = mix(style.color, bg, if (greyed) 80 else 256)
		val alignedRect = roundBottomRight(bounds)
		gc.setBackground(colorForRGB(bg))
		gc.setForeground(colorForRGB(fg))
		gc.setFont(font(style.fontFamily, style.fontSize, style.fontStyle))
		val fm = gc.getFontMetrics
		val h = fm.getAscent + fm.getDescent
		val y = (bounds.height - h) / 2
		val w = gc.stringExtent(value).x
		val x =
			if (style.textAlign == TextAlignLeft)
				style.marginLeft

			else if (style.textAlign == TextAlignCenter)
				(bounds.width - w) / 2

			else if (style.textAlign == TextAlignRight)
				bounds.width - style.marginRight - w

			else
				error("Unrecognised textAlign value "+style.textAlign)

		// Only set clipping if we need to:
		if (!bounds.isEmpty) {
			if (x < 0 || y < 0 || x+w >= bounds.width || y + h >= bounds.height)
				// Clip to exactly top-left and slightly beyond bottom-right:
				withclip(alignedRect){
					// Because we have clipped to exact top-left coordinate, fill to beyond
					// top-left to ensure that we don't leave a gap.
					gc.fillRectangle(roundTopLeft(alignedRect))
					gc.drawString(value, bounds.x+x, bounds.y+y, true)
				}
			else {
				// Fill from exactly top-left and slightly beyond bottom-right:
				gc.fillRectangle(alignedRect)
				gc.drawString(value, bounds.x+x, bounds.y+y, true)
			}
		}
	}

	def mixBackground(bg: RGB, highlightIntensity: Int): RGB = highlightIntensity match {
		case 0 => bg
		case 2 => highlightRGB
		case 1 => mix(bg, highlightRGB, 180)
	}

	def mix(one: RGB, two: RGB, alpha: Int) = {
		require(alpha >= 0 && alpha <= 256)
		alpha match {
			case 0 => two
			case 256=>one
			case _ =>
				val other = (256 - alpha)
				new RGB(
					((one.red * alpha) + (two.red * other)) >> 8,
					((one.green * alpha) + (two.green * other)) >> 8,
					((one.blue * alpha) + (two.blue * other)) >> 8);
		}
	}

	private def roundBottomRight(r: Rectangle) =
		new Rectangle(r.x, r.y, roundUp(r.width), roundUp(r.height))

	private def roundTopLeft(rect: Rectangle) = {
		val b = rect.y + rect.height
		val r = rect.x + rect.width
		val t = roundDown(rect.y)
		val l = roundDown(rect.x)
		new Rectangle(l, t, (r - l), (b - t))
	}


}
