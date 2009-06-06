/*
 * LineDescriptor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.gfx

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._

class LineDescriptor(var colour: RGB, var thickness: Float, var style: Int){

	def this(colour: RGB, thickness: Float) = this(colour, thickness, SWT.LINE_SOLID)

	private lazy val attrs = new LineAttributes(thickness, SWT.CAP_SQUARE, SWT.JOIN_MITER, style, null, 0, 0)

	def setAttributesOf(gfx: Gfx){
		import gfx._
		gc setLineAttributes attrs
		gc setForeground colorForRGB(colour)
	}
}
