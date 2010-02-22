/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._
import org.eclipse.swt.widgets._
import actions.InapplicableActionException
import Listeners._

class OverlayNode(parent: Composite) {
	val shell = new Canvas(parent, SWT.NONE)

	val listener: Listener = handleEvent _
	Seq(
		SWT.Dispose, SWT.Paint, SWT.FocusIn, SWT.Activate,
		SWT.MouseDown, SWT.MouseUp,
		SWT.MouseEnter, SWT.MouseExit,
		SWT.Help)
		.foreach(e => shell.addListener(e, listener))
		
	var armed = false
	var focused = false
	var bounds: Rectangle = null
	var glyph: NodeGlyph = null
	var action: Function0[Unit] = null

	def setup(newGlyph: NodeGlyph, newAction: Function0[Unit]) {
		glyph = newGlyph
		action = newAction

		val region = glyph.region
		bounds = region.getBounds
		shell.setRegion(region)
		shell.setSize(bounds.x + bounds.width, bounds.y + bounds.height)
		armed = false
		focused = false
	}

	
	def visible = shell.getVisible
	def visible_=(v: Boolean) {
		shell.setVisible(v)
	}

	def setVisible(v: Boolean) {shell.setVisible(v)}

	def location = shell.getLocation
	def location_=(p: Point) {
		setLocation(p.x, p.y)
	}
	def setLocation(x: Int, y: Int) {
		shell.setLocation(x - bounds.x, y - bounds.y)
	}


	def handleEvent(e: Event) {
		e.`type` match {
			case SWT.Dispose => onDispose(e)
			case SWT.Paint => glyph.onPaint(e.gc, armed, focused)
			case SWT.MouseDown =>
				armed = true
				shell.redraw

			case SWT.MouseUp =>
				if (armed) performAction
				focused = armed
				armed = false
				shell.redraw

			case SWT.MouseExit =>
				if (armed) {
					armed = false
					shell.redraw
				}else{
					focused = false
					shell.redraw
				}

			case SWT.MouseEnter =>
				if ((e.stateMask & SWT.BUTTON1) != 0) {
					armed = true
					shell.redraw
				}else{
					focused = true
					shell.redraw
				}

			case SWT.Activate =>
				// We don't want this window to itself ever be 'active'. It's just
				// a wee overlay control.
				//parent.getShell.setActive
				
			case _ => // Ignore anything else.
		}
	}

	def performAction = try {
			action.apply
		}catch{
			case _:InapplicableActionException => parent.getDisplay.beep
		}

	def onDispose(e: Event) {
		// Nothing to do.
		// ‘shell’ will be disposed when parent window is disposed.
	}
}
