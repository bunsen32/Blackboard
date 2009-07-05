/*
 * Env.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import blackboard.data.types._

abstract class Env {
	def defs(key: String): Identifier
	def typeDefs(key: String): Type
}

object NullEnv extends Env {
	def defs(key: String) = throw new NoSuchElementException("Not found: " + key)
	def typeDefs(key: String) = throw new NoSuchElementException("Not found: " + key)
}

class NestedEnv(val outer: Env) extends Env {
	var localDefs: Map[String, Identifier] = Map.empty
	var localTypeDefs: Map[String, Type] = Map.empty

	def defs(key: String) =
		localDefs.getOrElse(key, outer.defs(key))
	def typeDefs(key: String) =
		localTypeDefs.getOrElse(key, outer.typeDefs(key))
}
