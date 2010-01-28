/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 08-Jan-2010
 * Time: 19:28:51
 * To change this template use File | Settings | File Templates.
 */

package net.dysphoria.blackboard.ui.model

import org.eclipse.swt.graphics._
import net.dysphoria.blackboard._
import ui._
import gfx._
import selection._
import collection.immutable.Map

abstract class TableArray extends TablePart {
	def showEmptyAxes = true

	def computeInnerSizeAndHeaders {
		val width = sizeOf(Horizontal, xAxes)
		val height = sizeOf(Vertical, yAxes)
		innerSize = new Point(width, height)
		topHeader = (0 /: xAxes)(_ + xLabelHeight(_, 0))
		leftHeader = (0 /: yAxes)(_ + yLabelWidth(_, 0))
	}

	def sizeOf(orientation: Orientation, axes: Seq[Axis]): Int = {
		if (!axes.isEmpty) {
			val remainingAxes = axes.drop(1)
			axes(0) match {
				case a: ArrayAxis =>
					val breadth = sizeOf(orientation, remainingAxes)
					breadth * visibleLength(a) // unit breadth × size of dimension

				case _ => error("TableArray contains non-Array Axis!")
			}

		}else{
			breadthOfCell(orientation, null)
		}
	}

	def labelDepth(labelOrientation: Orientation, a: Axis, i: Int) =
		if (labelOrientation.isX) xLabelHeight(a, i) else yLabelWidth(a, i)

	/*------------------------------------------------------------------------*/
	// SIZING

	def breadthCellBounds(offset: Int, o: Orientation, coords: Map[Axis,Int]): Range = {
		val breadth = breadthOfCell(o, coords) // Assume all cells same width
		val b0 = cellIndexOf(o, coords) * breadth + offset
		val b = 1 * breadth // Cell count must == 1
		new Range(b0, (b0 + b), 1)
	}


	def breadthOwnLabelBounds(offset: Int, o: Orientation, coords: Map[Axis,Int], num: Int) = {
		val i = cellIndexOf(o, coords)
		val breadth = breadthOfCell(o, coords) // Assume all cells same width
		val b0 = offset + (i * breadth)
		new Range(
			b0,
			b0 + (num * breadth), 1)
	}


	def childLabelBounds(dataOrigin: Point, lab: LabelInstance) =
		error("childLabelBounds: TableArray does not have child elements.")

	override type Instance <: TableArrayInstance

	abstract class TableArrayInstance extends TablePartInstance {
		override def model = TableArray.this
		type ComponentType <: AbstractCellInstance

		/*------------------------------------------------------------------------*/
		// NAVIGATION (self)

		def edgeCell(edge: CompassPosition)(implicit hints: SelectionHints): Option[SingleGridSelection] =
			Some(cell(coords
					++ axesEnd(edge.orientation, edge.end)
					++ hints.forAxes(axes(edge.orientation.other)) ))

		/*------------------------------------------------------------------------*/
		// NAVIGATION (component elements)

		def oneBeyond(child: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints): Option[SingleGridSelection] =
			adjacentChild(child, direction) orElse container.oneBeyondContent(this, direction)

		def oneBeyondContent(child: ComponentType, direction: CompassPosition)(implicit hint: SelectionHints) =
			oneBeyond(child, direction)

		def oneBeyondLabelArea(childPartLabel: UnnestedLabelInstance, direction: CompassPosition)(implicit hint: SelectionHints) =
			error("oneBeyondLabel area not applicable to TableArray")

	}

	def edgeLabel(container: LabelContainer, whichLabels: Orientation, edge: CompassPosition)(implicit hint: SelectionHints) =
		ownedEdgeLabel(container, whichLabels, edge)

	def firstChildLabelOf(container: LabelInstance)(implicit hint: SelectionHints) = None

	/*------------------------------------------------------------------------*/
	// HIT TESTING

	// Don't have any child labels, so just return selection as-is
	def hitTestChildLabels(parent: LabelInstance, b: Int, d: Int) =
		NullSelection

	def hitTestAxis(o: Orientation, b: Int): Option[(Map[Axis,Int], Int)] = {
		var remainder = b
		var available = innerBreadth(o)
		var axesCoords: Map[Axis, Int] = Map.empty
		for(ax <- axes(o)) {
			val perValue = available / visibleLength(ax)
			val v = remainder / perValue
			axesCoords += ((ax, v))
			remainder %= perValue
			available = perValue
		}
		Some((axesCoords, remainder))
	}

	def containsInEdgeArea(label: LabelInstance) = (label.ownerPartModel == this)

}