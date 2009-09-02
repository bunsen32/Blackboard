/*
 * Compiler.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.util.parsing.input.Reader

object Compiler extends CompilationPhase[Reader[Char], Ast.Node] {

	val AllPhases =
		GrammarParser chain
		NameResolver chain
		EvaluationResolver chain
		TraitResolver chain
		Typer

	def process(in: Reader[Char], env: Environment) = AllPhases.process(in, env)
}
