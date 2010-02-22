/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.gfx

import org.eclipse.swt.graphics._

object Style {

	val BlockOutline = new LineDescriptor(new RGB(90, 90, 120), 3F)
	val BlockFill = new RGB(90, 90, 120)
	val BlockFillAlpha = 128

	val DropInsertionLine = new LineDescriptor(new RGB(255, 0, 0), 2F)

	// Probably these don't belong here, but will have to wait until I create
	// some proper colour-mixing/compositing classes:

	def mix(a: RGB, factor: Float, b: RGB) = {
		require(factor >= 0F && factor <= 1F)
		val inverse = 1F - factor
		new RGB(
			(a.red * inverse + b.red * factor).toInt,
			(a.green * inverse + b.green * factor).toInt,
			(a.blue * inverse + b.blue * factor).toInt)
	}
}
