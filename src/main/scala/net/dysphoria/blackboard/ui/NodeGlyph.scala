/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics.{Region, GC, Device}

abstract class NodeGlyph(device: Device) extends Disposable {
	lazy val region = {
		val r = new Region(device)
		setupRegion(r)
		r
	}

	def setupRegion(r: Region)

	def onPaint(gc: GC, armed: Boolean, focused: Boolean)

	def dispose {
		region.dispose
	}
}
