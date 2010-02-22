/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

object Precedence {
	def ofTernaryOperator = 0
	def of(ident: String): Int = ident(0) match {
		case c if Character.isLetter(c) => 1 // (SECOND-)LOWEST PRECEDENCE
		case '|' => 2
		case '^' => 3
		case '&' => 4
		case '<' | '>' | '≥' | '≤' => 5
		case '=' | '!' => 6
		case ':' => 7
		case '+' | '-' => 8
		case '*' | '/' | '%' | '×' | '÷' => 9
		case _ => 10            // HIGHEST PRECEDENCE (other symbols)
	}
	def ofFunctionApplication = 11
}
