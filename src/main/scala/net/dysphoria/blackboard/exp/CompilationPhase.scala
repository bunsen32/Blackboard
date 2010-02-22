/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

trait CompilationPhase[Input, Output] {
	type E = Environment

	def process(i: Input, env: E): Either[Seq[LanguageError], Output]

	def chain[OutputB](other: CompilationPhase[Output, OutputB]) =
		new CompilationPhase[Input, OutputB] {
			def process(i: Input, e: E) =
				CompilationPhase.this.process(i, e) match {
					case Left(fail) => Left(fail)
					case Right(succ) => other.process(succ, e)
				}
		}
}
