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

trait IsBlockLevel extends HasDisplayBlocks

case class SingleGridSpace(grid: MetaGrid, ix: Int, iy: Int) extends HasDisplayBlock with IsBlockLevel {
	override def block = grid(ix, iy)
	override def contains(other: Selectable) = other match {
		case b: HasDisplayBlock => block == b.block
		case _ => super.contains(other)
	}
}

