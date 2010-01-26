/*
 * MutatorOverlay.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Event}
import org.eclipse.swt.graphics.Rectangle
import Listeners._

/**
 * Displays a set of overlay icons for insertions/
 */
class SelectionEditingOverlay(val control: ViewCanvas) extends Disposable {
	def stdSize = 17

	private val _nodePool = new mutable.ArrayBuffer[OverlayNode]
	private var _usedNodes: Set[OverlayNode] = Set.empty
	private var _visible = false
	private var _parentFocused = true
	private var _rect: Rectangle = null

	control.geometryChangedListeners += (_ => reposition)
	control.addListener(SWT.FocusOut, controlListener _)
	control.addListener(SWT.FocusIn, controlListener _)

	var up: List[OverlayNode] = Nil
	var down: List[OverlayNode] = Nil
	var left: List[OverlayNode] = Nil
	var right: List[OverlayNode] = Nil

	def visible = _visible
	def visible_=(v: Boolean) {
		_visible = v
		checkRealVisibility
	}

	def reallyVisible = _visible & _parentFocused

	def rectangle = _rect
	def rectangle_=(r: Rectangle) {
		_rect = r
		reposition
	}

	def set(rect: Rectangle, nodes: Seq[EditingNodeSpec]){
		visible = false
		_rect = rect

		ensureEnoughNodes(nodes.length)
		var i = 0
		_usedNodes = Set.empty
		up = Nil; down = Nil; left = Nil; right = Nil
		for(s <- nodes.reverse){
			val n = _nodePool(i)
			n.setup(s.glyph, s.f)
			s.position match {
				case Up => up ::= n
				case Down => down ::= n
				case Right => right ::= n
				case Left => left ::= n
			}
			_usedNodes += n
			i += 1
		}
		visible = true
	}

	def checkRealVisibility {
		reposition
		for(n <- _nodePool)
			n.visible = _usedNodes.contains(n) && reallyVisible
	}

	def reposition {
		if (reallyVisible) {
			val topLeft = control.modelToView(_rect.x, _rect.y)
			val bottomRight = control.modelToView(_rect.x + _rect.width, _rect.y + _rect.height)
			def layOut(nodes: Seq[OverlayNode], x0: Int, y0: Int, dx: Int, dy: Int){
				var x = x0; var y = y0
				for(n <- nodes){
					n.setLocation(x, y)
					x += dx; y += dy
				}
			}

			layOut(up, topLeft.x, topLeft.y - stdSize, stdSize, 0)
			layOut(right, bottomRight.x, topLeft.y, 0, stdSize)
			layOut(left, topLeft.x - stdSize, topLeft.y, 0, stdSize)
			layOut(down, topLeft.x, bottomRight.y, stdSize, 0)
		}
	}

	def controlListener(e: Event){
		e.`type` match {
			case SWT.FocusIn => _parentFocused = true; checkRealVisibility
			case SWT.FocusOut => _parentFocused = false; checkRealVisibility
			case _ => //ignore
		}
	}

	def ensureEnoughNodes(number: Int) {
		while(_nodePool.length < number)
			_nodePool += new OverlayNode(control)
	}

	def dispose {
		// Do nothing at the mo’. Elements in nodePool will be disposed when control is disposed.
	}
}
