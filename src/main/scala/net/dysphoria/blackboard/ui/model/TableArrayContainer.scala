/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.model

import net.dysphoria.blackboard.gfx.DrawingContext
import org.eclipse.swt.graphics.{Point, Rectangle}
import collection.immutable.Map
import net.dysphoria.blackboard.ui.{Displayable, Orientation, Axis}
import net.dysphoria.blackboard.ui.selection.{TableBuildingBlock, ContainerCellInstance, TableItemSelection}

class TableArrayContainer(var contents: Displayable) extends TableArray {
	def arrayTable(coords: Map[Axis, Int]) = null

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]) =
		orientation.breadth(contents.size)

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int]) =
		contents.render(gfx, new Point(bounds.x, bounds.y), indices)

	override type Instance = TableArrayContainerInstance

	def instance(container: ContainingType, coords: Map[Axis, Int]) = new TableArrayContainerInstance(container, coords)

	case class TableArrayContainerInstance(container: ContainingType, coords: Map[Axis, Int]) extends TableArrayInstance {
		override def model = TableArrayContainer.this
		type ComponentType = ContainerCellInstance
		def hitTestCell(cellCoords: Map[Axis, Int], relative: Point) = cell(cellCoords).contents.hitTest(relative)
		def cell(cellCoords: Map[Axis, Int]) = ContainerCellInstance(this, cellCoords)
	}

}