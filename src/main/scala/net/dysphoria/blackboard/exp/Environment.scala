/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

import net.dysphoria.blackboard
import blackboard.data.types._
import Ast._

/**
 * A notion of the already-compiled functions and types. That is, those which may
 * be assumed and used by the code currently being compiled. Practically this will
 * always include BuiltIn types and functions (see BuiltInEnv), and may include
 * others imported implicitly or explicitly by the module being compiled.
 */
abstract class Environment {
	def values: PartialFunction[String, ValueIdentifier]
	def types: PartialFunction[String, TypeIdentifier]

	def ids: Map[Identity, ValueIdentifier]
	def valueById(id: Identity): ValueIdentifier
}

/**
 * The Environment consisting only of built-in functions and types.
 */
object BuiltInEnv extends Environment {
	val values = BuiltIn.functions
	val types = BuiltIn.types

	val ids = Map.empty[Identity, ValueIdentifier] ++ (
		for(n <- BuiltIn.functionList) yield (n.identity->n))
	def valueById(id: Identity) = ids(id)
}