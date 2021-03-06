/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

import scala.util.parsing.input.Position

case class CompilationException(problem: LanguageError) extends Exception(problem.message)

abstract class LanguageError{
	def position: Position
	def message: String
}
case class SyntaxError(position: Position, message: String) extends LanguageError

abstract class SemanticError extends LanguageError{
	def node: Ast.Node
	def position = node.position
}
case class ParseError(node: Ast.Node, message: String) extends SemanticError

import net.dysphoria.blackboard
import blackboard.data.{types=>t}

abstract class TypeError extends SemanticError
case class TypeMismatchError(node: Ast.Node, expected: t.Type, received: t.Type) extends TypeError{
	def message = "Type mismatch: expected "+expected.toString+" but found "+received.toString
}
case class RecursiveTypeError(node: Ast.Node, typ: t.Type, container: t.Type) extends TypeError {
	def message = "Recursive unification: "+typ.toString+" cannot equal "+container.toString
}

case class RecursiveTraitError(node: Ast.TraitDef, seq: Seq[Ast.TraitDef]) extends TypeError {
	def message = "Trait extends itself: "+ (seq map (_.name)).mkString(" extends ")
}