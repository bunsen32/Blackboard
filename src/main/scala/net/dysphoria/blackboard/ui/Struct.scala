/*
 * Struct.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

abstract class Struct extends Aggregate {
	def axes: Seq[Axis] // Axes common to all elements + elementAxis
	def elementAxis: StructAxis
	def elements: Seq[Block]

}
