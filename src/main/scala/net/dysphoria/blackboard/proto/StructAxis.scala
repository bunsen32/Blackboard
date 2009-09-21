/*
 * StructAxis.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import data.{types=>t}

/**
 * The 'type' of a structure.
 */
abstract class StructAxis extends Axis {
	//class Element(var name: String)

	var elements: Seq[String] = Nil
	def label(i: Int) = elements(i)
	def length = elements.length
}
