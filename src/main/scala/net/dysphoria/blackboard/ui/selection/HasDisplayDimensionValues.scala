/*
 * DisplayDimensionSelectable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.selection

trait HasDisplayDimensionValues extends Selectable {
	def dimensionValues: Map[DisplayDimension, Set[Int]]

	override def contains(other: Selectable) = false //TODO
}


trait HasDisplayDimensionValue extends HasDisplayDimensionValues {
	
}
