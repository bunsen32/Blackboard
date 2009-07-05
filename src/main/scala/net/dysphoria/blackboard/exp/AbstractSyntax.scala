/*
 * AbstractSyntax.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import blackboard.data.types._

abstract class ALLPHASES
abstract class PARSE extends ALLPHASES
abstract class RESOLV extends ALLPHASES

sealed abstract class Exp[-P<:ALLPHASES] {
	var declaredType: Option[Type] = None
	override def toString = ExpressionToString(this)
}
case class If[-P<:ALLPHASES](pred: Exp[P], ifTrue: Exp[P], ifFalse: Exp[P]) extends Exp[P]
case class Match[-P<:ALLPHASES](param: Exp[P], cases: Case[P]*) extends Exp[P]
case class TupleExp[-P<:ALLPHASES](terms: Exp[P]*) extends Exp[P]
abstract class ScopedExp[-P<:ALLPHASES](val ids: Identifier*) extends Exp[P] {
	type ScopeType[-A<:ALLPHASES] <: ScopedExp[A]
	def body: Exp[P]
}
case class Scope[-P<:ALLPHASES](body: Exp[P], defn: Defn*) extends ScopedExp[P](defn:_*) {
	type ScopeType[-A<:ALLPHASES] = Scope[A]
}
case class Lambda[-P<:ALLPHASES](body: Exp[P], params: Param*) extends ScopedExp[P](params:_*) {
	type ScopeType[-A<:ALLPHASES] = Lambda[A]
}
case class TernaryTail[-P<:ALLPHASES](ifTrue: Exp[P], ifFalse: Exp[P]) extends Exp[P] {
	val precedence = 0
}

// Used unchanged in all phases
case class Const[V](val value: V) extends Exp[ALLPHASES]

// Used only in initial parse:
case class Evaluation(terms: List[Exp[PARSE]]) extends Exp[PARSE]
case class Dereference(ident: String) extends Exp[PARSE]

// The above are consumed by the NameResolver to produce:
case class Apply(function: Exp[RESOLV], argument: Exp[RESOLV]) extends Exp[RESOLV]
case class ResolvedRef(ident: Identifier) extends Exp[RESOLV] {
	def operator = ident match {
		case fn: FunctionDefn => fn.operator
		case _ => false
	}

	def postfix = ident match {
		case fn: FunctionDefn => fn.postfix
		case _ => false
	}

	def infix = ident match {
		case fn: FunctionDefn => fn.infix
		case _ => false
	}
}


case class Case[-P<:ALLPHASES](pattern: Pattern[P], result: Exp[P])
case class Pattern[-P<:ALLPHASES]
abstract class Identifier(val name: String, val typ: Type) {
	assume(name.length > 0)
}
class Param(name: String, typ: Type) extends Identifier(name, typ) {
	override def toString = name
}
class Defn(name: String, typ: Type, val parsedExp: Exp[PARSE]) extends Identifier(name, typ) {
	override def toString = "var "+name+" = "+(_resolvedExp.getOrElse(parsedExp)).toString
	private var _resolvedExp: Option[Exp[RESOLV]] = None
	def resolvedExp = _resolvedExp match {
		case None => throw new RuntimeException("Expression is not yet resolved")
		case Some(exp)=> exp
	}
	def resolvedExp_=(exp: Exp[RESOLV]) = if (_resolvedExp.isDefined)
			throw new RuntimeException("Expression is already resolved")
		else
			_resolvedExp = Some(exp)
}
class FunctionDefn(name: String, val operator: Boolean, typ: Function, exp: Exp[PARSE])
	extends Defn(name, typ, exp){

	val arity = typ.args.length
	assert((arity == 1 || arity == 2) || !operator)
	def postfix = operator && arity == 1
	def infix = operator && arity == 2
}


object ExpressionToString {

	def withParens[P<:ALLPHASES](exp: Exp[P]): String = {
		val result = apply(exp)
		if (result(0)=='(')
			result
		else
			"("+result+")"
	}

	def apply[P<:ALLPHASES](exp: Exp[P]): String = exp match {
		case Dereference(ident) => ident
		case If(pred, a, b) => "if "+withParens(pred)+" "+apply(a)+" else "+apply(b)
		case Match(param, cases@_*) => apply(param)+" match {}"
		case Scope(result, defs@_*) => "{" + defs.mkString("\n") + apply(result)+"}"
		case Lambda(body, params@_*) => "function("+params.mkString(",")+") = "+apply(body)
		case Evaluation(terms) => terms.mkString(" ")
		case TernaryTail(x, y) => "? "+x.toString+" : "+y.toString
		case TupleExp(terms @ _*) => terms.mkString("(", ", ", ")")
		case Const(a:String) => '"' + a + '"'
		case Const(a) => a.toString
		case Apply(fn, args) => apply(fn)+withParens(args)
		case ResolvedRef(ident) => ident.name
		case e => error("Other: "+e.getClass.toString)
	}
	
}

object Precedence {
	def ofTernaryOperator = 0
	def of(ident: Identifier): Int = ident.name(0) match {
		case c if Character.isLetter(c) => 1 // (SECOND-)LOWEST PRECEDENCE
		case '|' => 2
		case '^' => 3
		case '&' => 4
		case '<' | '>' | '≥' | '≤' => 5
		case '=' | '!' => 6
		case ':' => 7
		case '+' | '-' => 8
		case '*' | '/' | '%' | '×' | '÷' => 9
		case _ => 10		// HIGHEST PRECEDENCE (other symbols)
	}
	def ofFunctionApplication = 11
}

