/*
 * TheDragOperation.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import org.eclipse.swt.SWT
import org.eclipse.swt.events._
import org.eclipse.swt.graphics._
import org.eclipse.swt.widgets._
import Listeners._

/**
 * Interface for (essentially) a drag source.
 */
abstract class DragClient {
	/**
	 * Where the drag comes from. In a sense the exact control doesn't matter;
	 * more important is which window (Shell) it belongs to.
	 */
	val control: Control
	/**
	 * Called when the drag starts. Will not be called again before 'stop' is
	 * called.
	 */
	def start(operation: DragOperation) {}
	/**
	 * Paint the drag object.
	 */
	def paint(gc: GC) {}
	/**
	 * The drag has moved to <var>offset</var> over control <var>destControl</var>.
	 * The client should deal with rendering any 'drag over' effect.
	 */
	def move(destControl: Control, offset: Point) {}
	/**
	 * The drag completed successfully. Client should complete the transfer of
	 * data. Destination will be the last place indicated by a call to 'move'.
	 * If 'commit' is called, there will have been at least one call to 'move'.
	 */
	def commit {}
	/**
	 * Remove any drag-over indication. The drag has finished. May or may not
	 * be preceded by a call to 'commit'. If there was no 'commit', the drag
	 * was cancelled.
	 */
	def stop {}
}

class DragOperation(val window: Shell) {
	import DragOperation.Origin
	private def display = window.getDisplay
	val dragLayer = new Shell(window, SWT.NO_TRIM | SWT.POP_UP)
	dragLayer setAlpha 192
	private val _region = new Region(display)
	private val _transform = new Transform(display)
	_transform.identity
	private var _someClient: Option[DragClient] = None

	private var _size: Point = Origin
	private var _rotationRadians: Float = 0F
	private var _origin: Point = Origin // derived from size+rotation
	private var _dragAnchor = Origin
	private var _transformedAnchor = Origin

	private val listener: Listener = handleEvent _
	dragLayer.addListener(SWT.Paint, listener)

	def dispose {
		dragLayer.dispose
		_region.dispose
		_transform.dispose
	}

	def start(newClient: DragClient) {
		require(!started)
		_someClient = Some(newClient)
		import newClient.control
		control.addListener(SWT.Dispose, listener)
		display.addFilter(SWT.MouseMove, listener)
		display.addFilter(SWT.MouseUp, listener)
		display.addFilter(SWT.KeyDown, listener)
		_rotationRadians = 0F // Reset these...
		_dragAnchor = Origin // ...They'll get refreshed by 'updateLayerShape'
		newClient.start(this)
		updateLayerShape
		dragLayer setVisible true
	}
	
	def stop {
		if (started) {
			client.stop
			val control = client.control
			control.removeListener(SWT.Dispose, listener)
			display.removeFilter(SWT.MouseMove, listener)
			display.removeFilter(SWT.MouseUp, listener)
			display.removeFilter(SWT.KeyDown, listener)
			dragLayer setVisible false
			dragLayer.setSize(10, 10) // reduce resource requirements
			_someClient = None
		}
	}

	def started = _someClient.isDefined
	def client = _someClient match {
		case Some(c) => c
		case None => throw new IllegalStateException("Drag operation not started. Cannot get client.")
	}

	def dragAnchor = _dragAnchor
	def dragAnchor_=(newDragAnchor: Point) {
		_dragAnchor = newDragAnchor
		transformDragAnchor
	}

	def size = _size
	def size_=(newSize: Point) = {
		if (_size != newSize){
			require(newSize.x >= 0 && newSize.y >= 0)
			_size = newSize
			updateLayerShape
		}
	}

	def rotationRadians = _rotationRadians
	def rotationRadians_=(radians: Float) {
		if (_rotationRadians != radians){
			_rotationRadians = radians
			updateLayerShape
		}
	}

	private def handleEvent(e: Event) {
		if (started) e.`type` match {
			case SWT.MouseMove => {
				updateLayerPosition
				// Possibly replace this with something to get the dest control,
				// if we ever get round to drag-and-drop between controls.
				val destCtrl = client.control
				val point = display.map(client.control, destCtrl, e.x, e.y)
				client.move(destCtrl, point)
			}
			case SWT.MouseUp => {
				client.commit
				stop
			}
			case SWT.KeyDown if (e.keyCode == SWT.ESC) => {
				// On Carbon, this won't fire during a drag. On Cocoa it will.
				stop
			}
			case SWT.Dispose => stop
			case SWT.Paint => {
				e.gc.setAdvanced(true)
				e.gc.setAntialias(SWT.ON)
				e.gc.setTransform(_transform)
				client.paint(e.gc)
			}
			case _ => ;//ignore
		}
	}

	private def updateLayerShape {
		if (rotationRadians == 0) {
			// Special-case the simple case: 0° rotation
			dragLayer.setSize(size)
			dragLayer.setRegion(null)
			_origin = Origin
			_transform.identity

		}else if (rotationRadians == (Math.Pi/2D).toFloat) {
			// Special case 90°
			dragLayer.setSize(size.y, size.x)
			dragLayer.setRegion(null)
			_origin = new Point(size.y, 0)
			_transform.identity
			_transform.translate(size.y, 0)
			_transform.rotate(Math.toDegrees(rotationRadians).toFloat)

		}else{
			// Fully general case
			// Doesn't look very nice (it's a bit jittery) so probably needs some
			// optimisation (e.g., keeping same size of window and rotating within it).
			// Or even some native code. I mean it's really for aesthetics anyway.
			val w = _size.x
			val h = _size.y
			val cos = Math.cos(rotationRadians)
			val sin = Math.sin(rotationRadians)
			val wcos = cos * w
			val wsin = sin * w
			val hcos = cos * h
			val hsin = sin * h

			val p = Array(
				0, 0,
				(wcos).toInt, (wsin).toInt,
				(wcos-hsin).toInt, (wsin+hcos).toInt,
				(-hsin).toInt, (hcos).toInt)
			val minx = p(0) min p(2) min p(4) min p(6)
			val miny = p(1) min p(3) min p(5) min p(7)
			for(i <- 0 until 4){
				p(i*2+0) -= minx
				p(i*2+1) -= miny
			}
			val maxx = p(0) max p(2) max p(4) max p(6)
			val maxy = p(1) max p(3) max p(5) max p(7)
			dragLayer.setSize(maxx, maxy)
			_region.subtract(0, 0, maxx, maxy)
			_region.add(p)
			dragLayer.setRegion(_region)
			_origin = new Point(p(0), p(1))
			_transform.identity
			_transform.translate(p(0), p(1))
			_transform.rotate(Math.toDegrees(rotationRadians).toFloat)
		}
		transformDragAnchor
		dragLayer.redraw
	}
	private def transformDragAnchor {
		val x = _dragAnchor.x
		val y = _dragAnchor.y
		val cos = Math.cos(rotationRadians)
		val sin = Math.sin(rotationRadians)
		_transformedAnchor = new Point(
			_origin.x + (cos * x - sin * y).toInt,
			_origin.y + (sin * x + cos * y).toInt)
		updateLayerPosition
	}
	private def updateLayerPosition {
		val mouse = display.getCursorLocation
		dragLayer.setLocation(
			mouse.x - _transformedAnchor.x,
			mouse.y - _transformedAnchor.y)
	}

}

/*
 * There should be only a single drag operation in effect in the application at
 * any one time.
 */
object DragOperation {
	private val Origin = new Point(0, 0) // never mutated, by convention
	var existingOperations = mutable.Map[Shell, DragOperation]()

	def start(from: DragClient) = {
		val fromWindow = from.control.getShell
		val op = if (existingOperations.contains(fromWindow)){
				val drag = existingOperations(fromWindow)
				drag.stop // In case there's an on-going drag operation
				drag
				
			}else {
				val drag = new DragOperation(fromWindow)
				existingOperations(fromWindow) = drag
				// If the window is disposed, remove the DragOperation from our cache.
				fromWindow.addDisposeListener((e: DisposeEvent) => {
					drag.stop
					drag.dispose
					existingOperations.removeKey(fromWindow): Unit
				})
				drag
			}
		op.start(from)
		op
	}

}
