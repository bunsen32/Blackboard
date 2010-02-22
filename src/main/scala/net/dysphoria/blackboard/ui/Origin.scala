/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Point

object Origin {
	private val point = new Point(0, 0)

	implicit def originToPoint(o: Origin.type): Point = {
		assert(point.x == 0 && point.y == 0)
		point
	}
}