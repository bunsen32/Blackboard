/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

import scala.collection.mutable
import net.dysphoria.blackboard._
import data.{types=>t}

class TestDataArray(val dimensions: Seq[ArrayAxis]) extends DataArray {
	val elementType = t.core.String

	val modifications = new mutable.ArrayBuffer[String]

	def apply(c: Map[Axis, Int]) = flatApply(flatten(c))

	def update(c: Map[Axis,Int], value: Any) = flatUpdate(flatten(c), value)

	protected def flatten(c: Map[Axis,Int]) = {
		var p = 0
		for(d<-dimensions){
			val i = c.getOrElse(d, error("Axis "+d+" is not provided"))
			p = (p * d.length) + i
		}
		p
	}
	def flatApply(p: Int) = {
		val s = if (p < modifications.length) modifications(p) else null
		if (s != null) s else strings(p % strings.length)
	}

	def flatUpdate(p: Int, str: Any) {
		val value = str.asInstanceOf[String]
		val missingElements = p + 1 - modifications.length
		if (missingElements > 0)
			modifications ++= Stream.make(missingElements, null:String)
		modifications(p) = value
	}

	val strings = Array("bloo", "fish", "wallash", "frumrock", "blloodgie", "daaaaa", "smbat!", "clartock", "virridian", "amanamana", "twelty arb")
}
