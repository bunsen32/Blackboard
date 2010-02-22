/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
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
