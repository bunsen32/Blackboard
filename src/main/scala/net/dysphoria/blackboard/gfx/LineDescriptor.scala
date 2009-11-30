/*
 * LineDescriptor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.gfx

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._

case class LineDescriptor(colour: RGB, thickness: Float, style: Int){
	require(style != SWT.LINE_CUSTOM) // No mechanism to pass custom dash offsets, so disallow it.

	def this(colour: RGB, thickness: Float) = this(colour, thickness, SWT.LINE_SOLID)

	lazy val lineAttributes = new LineAttributes(thickness, SWT.CAP_SQUARE, SWT.JOIN_MITER, style, null, 0, 0)
}
