/*
 * AbstractSyntax.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import blackboard.data.types._

/*
 * Slightly devious use of ("phantom"?) types to use Scala type system to distinguish
 * between expressions which are rawly parsed, and those which are resolved (and
 * have had their precedences sorted out). An Exp[PARSE] cannot refer to Exp[RESOLV]s
 * and vice versa.
 */

abstract class ANYPHASE
abstract class PARSE extends ANYPHASE
abstract class RESOLV extends ANYPHASE

sealed abstract class Exp[-P<:ANYPHASE] {
	var resolvedType: Option[Type] = None
	override def toString = ExpressionToString(this)
}
case class If[-P<:ANYPHASE](pred: Exp[P], ifTrue: Exp[P], ifFalse: Exp[P]) extends Exp[P]
case class Match[-P<:ANYPHASE](param: Exp[P], cases: Case[P]*) extends Exp[P]
case class TupleExp[-P<:ANYPHASE](terms: Exp[P]*) extends Exp[P]
abstract class ScopedExp[-P<:ANYPHASE](val ids: Identifier*) extends Exp[P] {
	type ScopeType[-A<:ANYPHASE] <: ScopedExp[A]
	def body: Exp[P]
	def env: Env = throw new InternalFault("Env not assigned")
}
case class Scope[-P<:ANYPHASE](body: Exp[P], defn: Defn*) extends ScopedExp[P](defn:_*) {
	type ScopeType[-A<:ANYPHASE] = Scope[A]
}
case class Lambda[-P<:ANYPHASE](body: Exp[P], params: Param*) extends ScopedExp[P](params:_*) {
	type ScopeType[-A<:ANYPHASE] = Lambda[A]
}

// Used unchanged in all phases
case class Const(val value: Any, val typ: Type) extends Exp[ANYPHASE]

// Used only in initial parse:
case class Evaluation(terms: List[Exp[PARSE]], ternaryTail: Option[TernaryTail]) extends Exp[PARSE]
case class TernaryTail(ifTrue: Exp[PARSE], ifFalse: Exp[PARSE])
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


case class Case[-P<:ANYPHASE](pattern: Pattern[P], result: Exp[P])
case class Pattern[-P<:ANYPHASE]
abstract class Identifier(val name: String, val typ: Type) {
	assume(name.length > 0)
}
class Param(name: String, typ: Type) extends Identifier(name, typ) {
	override def toString = name
}
class Defn(name: String, typ: Type, val parsedExp: Exp[PARSE]) extends Identifier(name, typ) {
	override def toString = "var "+name+" = "+(_resolvedExp.getOrElse(parsedExp)).toString
	private var _resolvedExp: Option[Exp[RESOLV]] = None
	def resolvedExp = _resolvedExp.getOrElse(throw new InternalFault("Expression is not yet resolved"))
	def resolvedExp_=(exp: Exp[RESOLV]) = if (_resolvedExp.isDefined)
			throw new InternalFault("Expression is already resolved")
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

	def withParens[P<:ANYPHASE](exp: Exp[P]): String = {
		val result = apply(exp)
		if (result(0)=='(')
			result
		else
			"("+result+")"
	}

	def apply[P<:ANYPHASE](exp: Exp[P]): String = exp match {
		case Dereference(ident) => ident
		case If(pred, a, b) => "if "+withParens(pred)+" "+apply(a)+" else "+apply(b)
		case Match(param, cases@_*) => apply(param)+" match {}"
		case Scope(result, defs@_*) => defs.mkString("{\n",";\n", ";\n"+apply(result)+"}")
		case Lambda(body, params@_*) => "function("+params.mkString(",")+") = "+apply(body)
		case Evaluation(terms, tail) => terms.mkString(" ") + (tail match {
			case None => ""
			case Some(TernaryTail(x, y)) => " ? "+x.toString+" : "+y.toString
		})
		case TupleExp(terms @ _*) => terms.mkString("(", ", ", ")")
		case Const(a:String, t) => '"' + a + '"'
		case Const(a, t) => a.toString
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

