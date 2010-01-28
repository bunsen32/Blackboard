package net.dysphoria.blackboard.ui.model

import scala.collection.mutable
import net.dysphoria.blackboard.gfx.DrawingContext
import org.eclipse.swt.graphics.{Rectangle, Point}
import collection.immutable.Map
import net.dysphoria.blackboard.ui.selection.{NullSelection, DataSelection}
import net.dysphoria.blackboard.ui._
import Origin._

/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 10-Jan-2010
 * Time: 22:02:18
 * To change this template use File | Settings | File Templates.
 */

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

	def boundsOf(origin: Point, ob: DataSelection) = {
		val (p, el) = elementFor(ob.unambiguousCoords)
		el.boundsOf(new Point(origin.x + p.x, origin.y + p.y), ob)
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

	override type Instance = GroupInstance

	def instance(coords: Map[Axis, Int]) = GroupInstance(coords)

	case class GroupInstance(coords: Map[Axis, Int]) extends DisplayableInstance {
		override def model = Group.this
		lazy val elements = (0 until model.elements.length) map (i => {
			val (position, d) = model.elements(i)
			val childCoords = coords + ((structAxis, i))
			(position, d.instance(childCoords))
		}) toSeq

		override def hitTest(point: Point) = {
			(0 until elements.length) map (i => {
				val (position, d) = elements(i)
				val relPoint = new Point(point.x - position.x, point.y - position.y)	
				d.hitTest(relPoint)
			}) find (_ != NullSelection) getOrElse NullSelection
		}

	}
}
