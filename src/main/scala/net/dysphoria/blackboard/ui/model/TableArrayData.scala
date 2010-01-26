/*
 * ArrayBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.model

import org.eclipse.swt.graphics._
import net.dysphoria.blackboard._
import ui._
import gfx._
import selection._
import collection.immutable.Map

class TableArrayData(val table: Table) extends TableArray {
	var array = new FlexibleDataArray(Nil)
	val cellStyle = new CellStyle
	val voidCellStyle = new CellStyle {
		backgroundColor = new RGB(200, 200, 230)
	}

	def arrayTable(coords: Map[Axis,Int]) = array

	def renderCell(gfx: DrawingContext, bounds: Rectangle, indices: Map[Axis, Int]){
		val withinData = !indices.exists(pair => pair._2 >= pair._1.length)
		val selected = gfx.ui.selection match {
			case DataCellInstance(_, coords) => if (coordinatesMatch(indices, coords)) 2 else 0
			case _=> 0
		}
		if (withinData){
			renderBasicCell(gfx, cellStyle, bounds,
							array(indices).toString, selected, false)
		}else{
			renderBasicCell(gfx, voidCellStyle, bounds, "", selected, false)
		}
	}

	def breadthOfCell(orientation: Orientation, c: Map[Axis, Int]): Int =
		orientation.choose(genericCellWidth, genericCellHeight)

	/*------------------------------------------------------------------------*/

	override type Instance = TableArrayDataInstance

	def instance(container: ContainingType, coords: Map[Axis, Int]) = new TableArrayDataInstance(container, coords)

	case class TableArrayDataInstance(container: ContainingType, coords: Map[Axis, Int]) extends TableArrayInstance {
		override def model = TableArrayData.this
		type ComponentType = DataCellInstance
		def hitTestCell(totalCoords: Map[Axis, Int], relative: Point) = cell(totalCoords)
		def cell(cellCoords: Map[Axis, Int]) = DataCellInstance(this, cellCoords)
	}


	override def toString = "TableArrayData"
}
