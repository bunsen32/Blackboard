/*
 * Displayable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import org.eclipse.swt.graphics._
import blackboard.gfx._

trait Displayable {
	def render(g: DrawingContext, xy: Point);
	def size: Point
}
