/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import selection.LabelInstance
import Origin._

class LabelEditor(label: LabelInstance) extends GridEditSource {
	def read = label.axis.label(label.index).toString
	def write(str: String) { label.axis.label_=(label.index, str) }
	def getBounds(canvas: ViewCanvas) = label.bounds
}
