/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.model

import scala.collection.mutable
import net.dysphoria.blackboard.gfx.DrawingContext
import org.eclipse.swt.graphics.{Rectangle, Point}
import collection.immutable.Map
import net.dysphoria.blackboard.ui.selection.{NullSelection, DataSelection}
import net.dysphoria.blackboard.ui._
import Origin._

class Group extends Displayable {
	var structAxis = new StructAxis(0)
	val elements = new mutable.ArrayBuffer[(Point, Displayable)]
	var origin: Point = Origin
	var size: Point = Origin

	def computeSize {
		var bounds = new Rectangle(0,0,0,0)
		for((p, d) <- elements) {
			d.computeSize
			val sz = d.size
			bounds.add(new Rectangle(p.x, p.y, sz.x, sz.y))
		}
		origin = new Point(bounds.x, bounds.y)
		size = new Point(bounds.width, bounds.height)
	}

	def render(g: DrawingContext, xy: Point, context: Map[Axis, Int]) {
		for(i <- 0 until elements.length) {
			val (p, d) = elements(i)
			d.render(
				g,
				new Point(xy.x + p.x, xy.y + p.y),
				context + ((structAxis, i)))
		}
	}

	def elementFor(coords: Map[Axis, Int]) = elements(coords(structAxis))

	def add(position: Point, element: Displayable) {
		structAxis.insert(structAxis.length, 1)
		elements.append((position, element))
	}

	def remove(element: Displayable) {
		elements.remove(indexOf(element))
	}

	def getRelativePosition(element: Displayable) =
		elements(indexOf(element))._1

	def setRelativePosition(element: Displayable, position: Point) {
		elements(indexOf(element)) = (position, element)
	}

	def indexOf(element: Displayable) =
		elements.findIndexOf(_._2 == element) match {
			case -1 => error("Displayable not found in group:"+element)
			case i => i
		}

	override type Instance = GroupInstance

	def instance(container: DisplayableContainer, coords: Map[Axis, Int]) = GroupInstance(container, coords)

	case class GroupInstance(val container: DisplayableContainer, coords: Map[Axis, Int]) extends DisplayableInstance with DisplayableContainer {
		override def model = Group.this

		override def hitTest(point: Point) = {
			((elements.length - 1) to 0 by -1) map (i => {
				val (position, d) = Group.this.elements(i)
				val relPoint = new Point(point.x - position.x, point.y - position.y)
				val size = d.size
				if (relPoint.x >= 0 && relPoint.x < size.x && relPoint.y >= 0 && relPoint.y < size.y)
					d.instance(this, coords + ((structAxis, i))).hitTest(relPoint)
				else
					NullSelection
			}) find (_ != NullSelection) getOrElse NullSelection
		}

		def positionOf(ob: DisplayableInstance) = {
			val (childPosition, childModel) = model.elementFor(ob.unambiguousCoords)
			assert(childModel == ob.model)
			val thisPosition = this.position
			new Point(thisPosition.x + childPosition.x, thisPosition.y + childPosition.y)
		}

	}
}
