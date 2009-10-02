/*
 * Axis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import gfx.LineDescriptor

abstract class Axis {
	def length: Int
	def range = 0 until length
	def label(i: Int): String
	def first = 0
	def last = length - 1

	var interItemLine: Option[LineDescriptor] = None
}
