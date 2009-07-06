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
	def defs(key: String) = throw new ParseError("Not found: " + key)
	def typeDefs(key: String) = throw new ParseError("Not found: " + key)
}

class NestedEnv(
	val outer: Env,
	val localDefs: Map[String, Identifier],
	val localTypeDefs: Map[String, Type]) extends Env {

	def defs(key: String) =
		localDefs.getOrElse(key, outer.defs(key))
	def typeDefs(key: String) =
		localTypeDefs.getOrElse(key, outer.typeDefs(key))
}


object NestedEnv {

	def apply[P<:ANYPHASE](outer: Env, s: ScopedExp[P]) = {
		var locals: Map[String, Identifier] = Map.empty
		for(d <- s.ids) {
			val name = d.name
			if (locals.contains(name))
				throw new ParseError("Duplicate definition of "+name)
			else
				locals += (name -> d)
		}
		new NestedEnv(outer, locals, Map.empty)
	}
}