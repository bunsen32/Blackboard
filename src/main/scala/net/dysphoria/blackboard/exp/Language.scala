/*
 * Language.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.util.parsing.combinator._
import blackboard.data.types._

class Language extends RegexParsers {
	def typeexp = typeterm~rep(rarrow~>typeterm) ^^
		{case (a~list) => composeFunctionType(a, list)}

	def typeterm: Parser[Type] = tupletype | typename
	
	def typename = "[_a-zA-ZΑ-Ωα-ω]\\w*".r ^^
		(new NamedVariable(_))
	
	def tupletype = "("~>repsep(typeexp, ",")<~")" ^^
		(list => list.lengthCompare(1) match {
			case x if x<0 => throw new ParseError("Empty brackets is not a valid type")
			case 0 => list(0)
			case x if x>0 => new Tuple(list: _*)
		})

	def composeFunctionType(head: Type, tail: List[Type]): Type = tail match {
		case Nil => head
		case x::xs => composeFunctionType(new Function(head, x), xs)
	}

	def rarrow: Parser[String] = "->" | "→"
	def rdarrow: Parser[String] = "=>" | "⇒"
}
