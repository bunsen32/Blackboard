/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

import scala.util.parsing.input._
import org.scalatest._
import org.scalatest.matchers._

class TestTokenisation extends FunSuite with ShouldMatchers {

	import Tokeniser._

	// Shmorgasboard of various tokens:
	def testAssorted {
		val a = Ident("a"); val b = Ident("b"); val c = Ident("c")
		val A = Ident("A"); val B = Ident("B")

		assertEqual(
			"function a[A, B]/* comment*/(a: A, b: B) = a+b; a=>c; ++++= /$*§ `function`", List(
			Function, a, OpenBracket, A, Comma, B, CloseBracket,
			  OpenParen, a, Colon, A, Comma, b, Colon, B, CloseParen,
			  Equals, a, Ident("+"), b, Semicolon, a, Implies, c, Semicolon,
			  Ident("++++="), Ident("/$*§"), Ident("function")
			))
	}

	def testEmptyTokeniser = assertEqual("", List())

	def testEmptyButWhitespaceTokeniser = assertEqual("\t//Stuff\n/*comment*/", List())

	def testFailsOnUnclosedComment {
		val lexer = new Scanner("/* some stuff 627ggd")
		lexer.first.getClass should be (classOf[ErrorToken])
		// (There may be tokens after this.)
	}

	def testSymbolsAreTerminatedByBlockComment {
		assertEqual("/&^^/*---*/", List(Ident("/&^^")))
	}

	def testSymbolsAreTerminatedByLineComment {
		assertEqual("/&**//&%", List(Ident("/&**")))
	}

	def testFailsOnUnclosedStrings {
		val lexer = new Scanner("' blah, blah, blah")
		lexer.first.getClass should be (classOf[ErrorToken])
		// (There may be tokens after this.)
	}

	def testIdentifiersCanStartOrEndWithReservedWords {
		assertEqual(
			"variable =< >= /-⇒ archetype", List(
			Ident("variable"), Ident("=<"), Ident(">="), Ident("/-⇒"), Ident("archetype")
		))
	}


	def lengthOf[A](r: Reader[A]): Int = if (r.atEnd) 0 else 1 + lengthOf(r.rest)

	def assertEqual(str: String, tokens: List[Token]){
		val lexer = new Scanner(str)
		if (!equal(lexer, tokens))
			assert(false, "Not equal:\n  "+str+"\n  "+ tokens.mkString(" ")
				   + "\nparsed as:\n  "+stringify(lexer))
	}

	def equal(actual: Reader[Token], expected: List[Token]): Boolean =
		actual.atEnd == expected.isEmpty && (
			actual.atEnd ||
				actual.first == expected.first &&
					equal(actual.rest, expected.tail))

	def stringify[A](reader: Reader[A]) = {
		def build(result: StringBuilder, r: Reader[A]) {
			result.append(r.first.toString)
			val rest = r.rest
			if (!rest.atEnd) {
				result.append(' ')
				build(result, rest)
			}
		}
		val result = new StringBuilder
		if (!reader.atEnd) build(result, reader)
		result.toString
	}

}

