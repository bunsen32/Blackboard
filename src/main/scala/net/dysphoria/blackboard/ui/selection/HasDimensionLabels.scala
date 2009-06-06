/*
 * DimensionLabelSelectable.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui.selection

import blackboard.data._
import scala.collection.immutable

trait HasDimensionLabels extends Selectable with HasDisplayBlocks with HasDisplayDimensionValues {
	def numberOfLabels: Int
	def isSingleLabel = numberOfLabels == 1

	implicit def toLabelSet: DimensionLabelsSelection
}

trait HasDimensionLabel	extends HasDimensionLabels with HasDisplayBlock {

	override def numberOfLabels = 1
	override def block: DimensionLabelsBlock
	def index: Int

	override def contains(other: Selectable) = other match {
		case other: HasDimensionLabel => (this.block == other.block && this.index == other.index)
		case _ => super.contains(other)
	}


	def to(other: HasDimensionLabel) = {
		require(this.block == other.block)
		new DimensionLabelsSelection(Map(block -> RangeIntSet((index min other.index) to (index max other.index))))
	}
}

case class SingleDimensionLabel(override val block: DimensionLabelsBlock, override val index: Int)
		extends HasDimensionLabel {

	override def dimensionValues = Map(block.displayDimension -> Set(index))
	override def toLabelSet = new DimensionLabelsSelection(Map(block -> Set(index)))
}


case class DimensionLabelsSelection(val blockIndexes: Map[DimensionLabelsBlock, Set[Int]])
		extends HasDimensionLabels with HasDisplayDimensionValues {

	override def block = throw new UnsupportedOperationException
	override def blocks = throw new UnsupportedOperationException
	override def isSingleBlock = false //TODO
	override def numberOfBlocks = 0 //TODO

	override def dimensionValues = Map.empty ++ (blockIndexes.map(kv => {val (b, v) = kv; (b.displayDimension, v)}))

	override lazy val numberOfLabels = (0 /: dimensionValues.values)(_ + _.size)
	override def toLabelSet = this

	override def contains(other: Selectable) = other match {
		case other: HasDimensionLabel => 
			blockIndexes.getOrElse(other.block, Set.empty) contains other.index

		case _ => super.contains(other)
	}

	def +(other: DimensionLabelsSelection): DimensionLabelsSelection =
		(this /: other.blockIndexes)(_ + _)

	def +(labels: (DimensionLabelsBlock, Set[Int])) = {
		val (block, newIndices) = labels
		val existingIndices = blockIndexes.getOrElse(block, RangeIntSet.empty)
		new DimensionLabelsSelection(blockIndexes.update(block, existingIndices ++ newIndices))
	}

	def -(other: DimensionLabelsSelection): DimensionLabelsSelection =
		(this /: other.blockIndexes)(_ - _)

	def -(labels: (DimensionLabelsBlock, Set[Int])) = {
		val (block, newIndices) = labels
		val existingIndices = blockIndexes.getOrElse(block, RangeIntSet.empty)
		if (existingIndices.isEmpty)
			this
		else
			new DimensionLabelsSelection(blockIndexes.update(block, existingIndices -- newIndices))
	}

	def ^(other: Selectable) = other match {
		case NullSelection => this
		case s: HasTableCells => this //TODO composition

		case DimensionLabelsSelection(other) => {
			val empty = Set.empty[Int]
			val newPairs = (blockIndexes.keySet ++ other.keySet) map (k => {
				val a = blockIndexes.getOrElse(k, empty)
				val b = other.getOrElse(k, empty)
				(k, (a excl b)++(b excl a))
			}) filter (pair => ! pair._2.isEmpty)

			if (newPairs.isEmpty)
				NullSelection
			else
				DimensionLabelsSelection(Map.empty ++ newPairs)
		}
	}
}

