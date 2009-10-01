/*
 * TestArray.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.proto

import data.{types=>t}

class TestArray(val dimensions: Seq[ArrayAxis]) extends ArrayTable {
	val elementType = t.core.String
	def flatApply(p: Int) = {
		strings(p % strings.length)
	}

	val strings = Array("bloo", "fish", "wallash", "frumrock", "blloodgie", "daaaaa", "smbat!", "clartock", "virridian", "amanamana", "twelty arb")
}
