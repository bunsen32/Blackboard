/*
 * LabelEditor.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

import selection.OneLabel

class LabelEditor(label: OneLabel) extends GridEditSource {
	def read = label.axis.label(label.index).toString
	def write(str: String) { label.axis.label_=(label.index, str) }

	def getBounds(canvas: ViewCanvas) = canvas.table.labelBounds(label)
}
