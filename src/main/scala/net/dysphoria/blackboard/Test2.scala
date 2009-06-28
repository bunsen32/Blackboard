/*
 * Test2.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard

import blackboard.exp._

object Test2 extends Language {


	def main(args: Array[String]) {
		Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))

		val typeExpressions = Array(
			"int -> bool",
			"(Double, Double, Double)",
			"(int->bool, bool->int, Double->Complex)",
			"(int, int, int)->string",
			"((β → γ) → ((γ → δ) → (β → δ)))"
		)
		for(exp <- typeExpressions){
			println(exp)
			println(parseAll(typeexp, exp))
		}
	}
}
