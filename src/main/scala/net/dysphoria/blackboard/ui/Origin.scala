package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.Point

/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 10-Jan-2010
 * Time: 17:47:35
 * To change this template use File | Settings | File Templates.
 */

object Origin {
	private val point = new Point(0, 0)

	implicit def originToPoint(o: Origin.type): Point = {
		assert(point.x == 0 && point.y == 0)
		point
	}
}