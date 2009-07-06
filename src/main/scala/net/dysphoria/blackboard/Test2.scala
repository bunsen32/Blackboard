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
		GrammarParser.parseExpression(in) match {
			case GrammarParser.Success(exp, _) => {
				println(exp)
				val resolved = NameResolver.resolve(exp)(BuiltIn)
				println(resolved)
				val typ = Typer.analyse(resolved)
				println(typ)
			}
			case failure => println(failure)
		}
	}
	
}
