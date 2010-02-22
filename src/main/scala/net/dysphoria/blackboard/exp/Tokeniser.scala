/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.lexical._

object Tokeniser extends Lexical with ConcreteSyntax with ImplicitConversions {

	implicit def charsToString(cs: List[Char]):String = cs.mkString("")
	implicit def charsToString(cs: ~[Char, List[Char]]):String = cs._1.toString + charsToString(cs._2)


	/**
	 * We allow most ASCII & Unicode symbols for identifiers. Notable exceptions are:
	 * > Syntax.Punctuation (as parsed by 'delim')
	 * > Unicode END_PUNCTUATION and START_PUNCTUATION (brackets)
	 * > DASH_PUNCTUATION, except for ASCII 'hyphen'
	 */
	def symbol = not(startOfComment)~>elem("symbol", c =>
		("-_?~!*^/&@:^%".indexOf(c) != -1) ||
		(Character.getType(c) match {
			case Character.CURRENCY_SYMBOL => true
			case Character.MATH_SYMBOL => true
			case Character.OTHER_SYMBOL=> true
			case _ => false
		}))

	def startOfComment = '/'~(accept('*')|'/')

	override def token: Parser[Token] = (
		'/'~'*' ^^^ ErrorToken("Unclosed block comment")
	|	delim
	|	symbol~rep(symbol) ^^ (a => makeIdentifier(a))
	|	letter~rep(letter|digit|'_')	^^ (a => makeIdentifier(a))
	|	realnumber
	|	digit~rep(digit) ^^ (a => DigitString(a))
	|	string('\'')
    |	string('"')
    |	'`' ~ rep(letter) ~ '`' ^^ { case '`' ~ chars ~ '`' => Ident(chars mkString "") }
    |	EofCh ^^^ EOF
    |	'\'' ^^^ ErrorToken("unclosed string literal")
    |	'\"' ^^^ ErrorToken("unclosed string literal")
    |	'`' ^^^ ErrorToken("unclosed quoted identifier")
    |	failure("illegal character")
	)
	
	lazy val delim: Parser[Token] = {
		def parseDelim(p: Punctuation) = accept(p.chars.toList) ^^ (str => p)
		val d = punctuation.toArray
		scala.util.Sorting.stableSort(d,
			(p1: Punctuation, p2: Punctuation)=>(p1.chars.length < p2.chars.length))

		d.toList.reverse.map(parseDelim).reduceRight(_ | _)
	}

	def string(delimiter: Char): Parser[CharString] =
		delimiter ~ rep(chrExcept(delimiter, '\n', EofCh) ) ~ delimiter ^^ {
			case st ~ chars ~ end => CharString(chars mkString "")
		}

	def realnumber = digit~rep(digit)~'.'~rep(digit) ^^ {
		case intpart~'.'~fracpart => RealNumberString(intpart+'.'+(fracpart.mkString))
	}

	// see `whitespace in `Scanners'
	override def whitespace: Parser[Any] = rep(
		whitespaceChar
    |	'/' ~ '*' ~> blockComment
    |	'/' ~ '/' ~> rep( chrExcept(EofCh, '\n') )
    )

	protected def blockComment: Parser[Any] = (
		'*' ~ '/'  ^^ { case _ => ' '  }
	|	chrExcept(EofCh) ~ blockComment
    )


	val reservedIdentifiersToTokens = Map.empty ++
		(for(k <- allKeywords) yield (k.chars -> k)) ++
		(for(k <- allKeywords; a <- k.alias) yield (a -> k))

	def makeIdentifier(s: String) = reservedIdentifiersToTokens.getOrElse(s, new Ident(s))

}
