/*
 * Token.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.util.parsing.syntax

trait ConcreteSyntax extends syntax.Tokens {

	case class Ident(override val chars: String) extends Token {
		require(chars.length != 0)
		override def toString = "Ident-"+chars
	}

	case class DigitString(override val chars: String) extends Token {
		override def toString = "Digits-"+chars
	}

	case class RealNumberString(override val chars: String) extends Token {
		override def toString = "Real-"+chars
	}

	case class CharString(override val chars: String) extends Token {
		override def toString = "\""+chars+"\""
	}

	class Keyword(override val chars: String, val alias: Option[String]) extends Token {
		def this(c: String) = this(c, None)
		override def toString = "Keywd-"+chars
	}
	case object Implicit extends Keyword("implicit")
	case object Function extends Keyword("function")
	case object Var extends Keyword("var")
	case object Equals extends Keyword("=")
	case object GoesTo extends Keyword("→", Some("->"))
	case object Implies extends Keyword("⇒", Some("=>"))
	case object Extends extends Keyword("extends")
	case object Type extends Keyword("type")
	case object Trait extends Keyword("trait")
	case object Treat extends Keyword("treat")
	case object As extends Keyword("as")
	case object For extends Keyword("for")
	case object Each extends Keyword("each")
	case object In extends Keyword("in")
	case object Question extends Keyword("?")
	case object Colon extends Keyword(":")
	case object If extends Keyword("if")
	case object Else extends Keyword("else")
	case object Match extends Keyword("match")
	case object True extends Keyword("true")
	case object False extends Keyword("false")
	case object Unit extends Keyword("unit")
	case object Prefix extends Keyword("prefix")
	case object SubtypeOf extends Keyword("<:")
	case object SupertypeOf extends Keyword(">:")

	case class Punctuation(override val chars: String) extends Token{
		override def toString = "Punct-"+chars
	}
	case object Dot extends Punctuation(".")
	case object Comma extends Punctuation(",")
	case object Semicolon extends Punctuation(";")
	case object OpenBracket extends Punctuation("[")
	case object OpenBrace extends Punctuation("{")
	case object OpenParen extends Punctuation("(")
	case object CloseParen extends Punctuation(")")
	case object CloseBrace extends Punctuation("}")
	case object CloseBracket extends Punctuation("]")

	val allKeywords = Set(Implicit, Function, Var, Equals, GoesTo, Implies, Extends, Type, Trait, Treat, As, For, In, Question, Colon, If, Else, Match, True, False, Unit, Prefix)
	val punctuation = Set(Dot, Comma, Semicolon, OpenBracket, OpenBrace, OpenParen, CloseParen, CloseBrace, CloseBracket)
}
