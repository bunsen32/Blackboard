/*
 * SelectableBlock.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.selection

trait HasDisplayBlocks extends Selectable {
	def numberOfBlocks: Int
	def isSingleBlock: Boolean
	def blocks: Set[DisplayBlock]
	def block: DisplayBlock

}

trait HasDisplayBlock extends HasDisplayBlocks {
	def numberOfBlocks = 1
	def isSingleBlock = true
	def blocks = Set(block)
}

case class SingleDisplayBlock(override val block: DisplayBlock) extends HasDisplayBlock {
	override def contains(other: Selectable) = other match {
		case b: HasDisplayBlock => blocks.contains(b.block)
		case _ => super.contains(other)
	}
}

case class EmptyGridSpace(val x: Int, val y: int) extends Selectable {

	override def contains(other: Selectable) = other match {
		case EmptyGridSpace(otherX, otherY) => x == otherX && y == otherY
		case _ => super.contains(other)
	}
}