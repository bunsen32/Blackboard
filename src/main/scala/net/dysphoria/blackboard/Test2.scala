/*
 * Test2.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard

import blackboard.exp._
import scala.util.parsing.input._

object Test2 {

	def dumpFromReader[A](r: Reader[A]) {
		if (! r.atEnd) {
			println(r.first.toString)
			dumpFromReader(r.rest)
		}
	}

	def main(args: Array[String]) {
		Console.setOut(new java.io.PrintStream(java.lang.System.out, true, "utf-8"))
		Console.setIn(new java.io.InputStreamReader(java.lang.System.in, "utf-8"))
		java.lang.System.setErr(new java.io.PrintStream(java.lang.System.err, true, "utf-8"))

		/*val typeExpressions = Array(
			"int -> bool",
			"(Double, Double, Double)",
			"(int ->bool, bool->int, Double-> Complex)",
			"(int, int, int)->string",
			"((β → γ) → ((γ → δ) → (β → δ)))"
		)
		for(exp <- typeExpressions){
			println(exp)
			println(typeexp(new lexical.Scanner(new CharSequenceReader(exp))))
		}*/

		val in = StreamReader(Console.in)
		Compiler.process(in, BuiltInEnv) match {
			case Left(errors) =>
					println("\nERRORS:")
					for(e <- errors)
						println(e.message + " at\n" + e.position.longString)
			case Right(tree) =>
				println("\nTYPED:")
				println(tree*Annotations.Type)
		}
/*		GrammarParser.parseExpression(in) match {
			case GrammarParser.Success(exp, _) => {
				println("PARSED:")
				println(exp)

				val (resolved, resErrors) = NameResolver.resolve(exp, BuiltInEnv)
				if (! resErrors.isEmpty){
					println("\nERRORS:")
					for(e <- resErrors)
						println(e.message + " at\n" + e.position.longString)
				}
				println("\nRESOLVED:")
				println(resolved)

				val (resolved2, resErrors2) = EvaluationResolver.resolve(resolved)
				if (! resErrors2.isEmpty){
					println("\nERRORS:")
					for(e <- resErrors2)
						println(e.message + " at\n" + e.position.longString)
				}
				println("\nEVAL TERMS RESOLVED:")
				println(resolved2)

				val res3 = TraitResolver.process(resolved2, BuiltInEnv)
				if (res3.isLeft){
					println("\nERRORS:")
					for(e <- res3.left.get)
						println(e.message + " at\n" + e.position.longString)
					return
				}
				println("\nTRAITS RESOLVED")
					
				val (typed, typErrors) = Typer.resolve((resolved2, res3.right.get), BuiltInEnv)
				if (! typErrors.isEmpty){
					println("\nERRORS:")
					for(e <- typErrors)
						println(e.message + " at\n" + e.position.longString)
				}
				println("\nTYPED:")
				println(typed*Annotations.Type)

				/*val result = TreeEvaluator.eval(resolved)
				println("\nEVALUATED:")
				println(result)*/
			}
			case failure => println(failure)
		}*/
	}
	
}
