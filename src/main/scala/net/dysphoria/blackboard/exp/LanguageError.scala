/*
 * Exceptions.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

case class CompilationException(problem: LanguageError) extends Exception(problem.message)

abstract class LanguageError{
	def where: Ast.Node
	def message: String
}
case class ParseError(where: Ast.Node, message: String) extends LanguageError

import blackboard.data.{types=>t}

abstract class TypeError extends LanguageError
case class TypeMismatchError(where: Ast.Node, expected: t.Type, received: t.Type) extends TypeError{
	def message = "Type mismatch: "+expected.toString+" ≠ "+received.toString
}
case class RecursiveTypeError(where: Ast.Node, typ: t.Type, container: t.Type) extends TypeError {
	def message = "Recursive unification: "+typ.toString+" cannot equal "+container.toString
}

case class RecursiveTraitError(where: Ast.TraitDef, seq: Seq[Ast.TraitDef]) extends TypeError {
	def message = "Trait extends itself: "+ (seq map (_.name)).mkString(" extends ")
}