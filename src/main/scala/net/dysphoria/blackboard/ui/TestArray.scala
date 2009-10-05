/*
 * TestArray.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.ui

import scala.collection.mutable
import data.{types=>t}

class TestArray(val dimensions: Seq[ArrayAxis]) extends ArrayTable {
	val elementType = t.core.String

	val modifications = new mutable.ArrayBuffer[String]

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
