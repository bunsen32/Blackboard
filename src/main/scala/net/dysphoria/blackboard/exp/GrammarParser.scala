/*
 * Language.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.TokenParsers
import blackboard.data.types

object GrammarParser extends TokenParsers {
	type Tokens = Tokeniser.type
	val lexical = Tokeniser

	def parseDefinitions(in: Reader[Char]) = {
		val lexer = new lexical.Scanner(in)
		phrase(definitions) (lexer)
	}

	def parseExpression(in: Reader[Char]) = {
		val lexer = new lexical.Scanner(in)
		phrase(exp) (lexer)
	}

	import lexical._

	// Definitions

	implicit def acc(t: Elem): Parser[Elem] = acceptIf(_ == t)("‘"+t+"’ expected, but "+ _ +" found")
	implicit def optionTypeToVar(optT: Option[types.Type]) =
		optT.getOrElse(new types.Variable)


	// Expressions
	type ParserExp = Parser[Exp[PARSE]]

	def exp: ParserExp = term~rep(term|ternarytail)~opt(Colon~>typeexp) ^^
		{case head~tail~opttyp => Evaluation(head::tail)}
	def term: ParserExp = (
		bracketed
	//|	seq_exp
		// Inside an expression, allow use of '=' as an operator:
	|	Equals ^^ {case Equals => Dereference("=")}
	|	deref
	|	value
	|	ifthenelse
	|	failure("Not a valid expression term"))


	def bracketed = OpenParen~>repsep(exp,Comma)<~CloseParen ^^ {
		case exps => exps.lengthCompare(1) match {
			case x if x<0 => Const(())
			case x if x==0=> exps(0)
			case x if x>0 => TupleExp(exps:_*)
		}
	}
	def bracketedsingleexp = OpenParen ~> exp <~ CloseParen
	def block = OpenBrace ~> exp <~ CloseBrace
	def deref = accept("name reference", {case Ident(n) => Dereference(n)})
	def ifthenelse = If~bracketedsingleexp~exp~Else~exp ^^
		{case If~cond~truePath~Else~falsePath => new If(cond, truePath, falsePath)}
	def ternarytail = Question~exp~Colon~exp ^^
		{case Question~x~Colon~y => TernaryTail(x, y)}

	def value = val_string | val_number
	def val_number = accept("number", {case DigitString(n) => Const(n.toInt)})
	def val_string = accept("string", {case CharString(n) => Const(n)})

	// For comprehensions

	def seq_exp = OpenBracket~>(for_comprehension|seq_elements)<~CloseBracket
	def for_comprehension = exp~rep1(comp_element)
	def comp_element = comp_for|comp_if
	def comp_for = For~>opt(Each)~>OpenParen~>formalparam~In~exp
	def comp_if = If~bracketedsingleexp
	
	def seq_elements = repsep(exp, Comma)


	def definitions = rep(defn_var | defn_fn | defn_infix_fn)

	// Variables

	def defn_var = Var~>defname~opt(typeannot)~Equals~exp ^^ {
		case name~typ~Equals~value => new Defn(name, typ, value)
	}

	// Functions and lambdas

	def defn_infix_fn = Function~>formalparamsingle~!defname~!formalparamsingle~!opt(typeannot)~!functionbody ^^ {
		case param1~name~param2~res~body => {
			val argtype = new types.Tuple(Array(param1.typ, param2.typ): _*)
			val params = Array(param1, param2)
			new FunctionDefn(name, true,
							 new types.Function(argtype, res),
							 Lambda(body, params: _*))
		}
	}
	def defn_fn = Function~>defname~!formalparamlist~!opt(typeannot)~!functionbody ^^ {
		case name~paramlist~res~body => {
			val argtype = paramtype(paramlist map (_.typ))
			new FunctionDefn(name, false,
							 new types.Function(argtype, res),
							 Lambda(body, paramlist: _*))
		}
	}
	
	def paramtype(params: Seq[types.Type]): types.Type =
		params.lengthCompare(1) match {
			case x if x<0 => types.Unit
			case x if x==0=> params(0)
			case x if x>0 => new types.Tuple(params: _*)
		}

	def defname = accept("name definition", {
			case Ident(n) => n
			case Equals => "=" // Allow '=' as a valid function name.
		})
	def formalparamsingle = OpenParen~>formalparam<~CloseParen
	def formalparamlist = OpenParen~>repsep(formalparam, Comma)<~CloseParen
	def formalparam = defname~opt(typeannot) ^^
		{case ident~optionalType => new Param(ident, optionalType)}
	def functionbody = Equals~>(exp|block) | block

	// Types

	/** A type annotation on an expression/variable/parameter/function. */
	def typeannot = Colon~>typeexp

	def typeexp = typeterm~rep(GoesTo~>typeterm) ^^
		{case (a~list) => composeFunctionType(a, list)}

	def typeterm: Parser[types.Type] = tupletype | typename
	
	def typename = accept("type name", {case Ident(n) => new types.NamedVariable(n)})
	
	def tupletype = OpenParen~>repsep(typeexp, Comma)<~CloseParen ^^
		(list => list.lengthCompare(1) match {
			case x if x<0 => throw new ParseError("Empty brackets are not a valid type")
			case 0 => list(0)
			case x if x>0 => new types.Tuple(list: _*)
		})

	def composeFunctionType(head: types.Type, tail: List[types.Type]): types.Type = tail match {
		case Nil => head
		case x::xs => composeFunctionType(new types.Function(head, x), xs)
	}

}
