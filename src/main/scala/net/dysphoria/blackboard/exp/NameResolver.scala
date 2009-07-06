/*
 * NameResolver.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

object NameResolver {


	def resolve(exp: Exp[PARSE])(implicit env: Env): Exp[RESOLV] = (exp: @unchecked) match  {
		case If(pred, a, b) => If(resolve(pred), resolve(a), resolve(b))
		case Match(param, cases@_*) => throw new UnsupportedOperationException
		case l: Lambda[_] => resolve(l)
		case s: Scope[_] => resolve(s)
		case e: Evaluation => resolveEvaluation(e)
		case Dereference(name) => ResolvedRef(env.defs(name))
		case TupleExp(terms @ _*) => TupleExp((terms map (resolve(_))): _*)
		case c: Const => c
		case _: Apply => error("Should not have any 'Apply's before RESOLV phase")
		case _: ResolvedRef => error("Should not have any 'ResolvedRef's before RESOLV phase")
	}

	def resolve(lambda: Lambda[PARSE])(implicit outer: Env): Lambda[RESOLV] = {
		var inner = NestedEnv(outer, lambda)
		new Lambda(resolve(lambda.body)(inner), lambda.params: _*){
			override def env = inner
		}
	}

	def resolve(scope: Scope[PARSE])(implicit outer: Env): Scope[RESOLV] = {
		var inner = NestedEnv(outer, scope)
		val newS = new Scope(resolve(scope.body)(inner), scope.defn: _*){
			override def env = inner
		}
		for(d <- newS.defn){
			d.resolvedExp = resolve(d.parsedExp)(inner)
		}
		newS
	}


	def resolveEvaluation(eval: Evaluation)(implicit env: Env): Exp[RESOLV] = {
		val Evaluation(unresolvedTerms, ternaryTail) = eval
		val terms = for(val u <- unresolvedTerms) yield resolve(u)
		
		val exp = orderEvaluation(terms)(Int.MinValue) match {
			case exp::Nil => exp
			case _ => throw new Exception("THAT shouldn't happen.")
		}
		ternaryTail match {
			case None => exp
			case Some(TernaryTail(ifTrue, ifFalse)) =>
				If(exp, resolve(ifTrue), resolve(ifFalse))
		}
	}

	def orderEvaluation(terms: List[Exp[RESOLV]])(implicit outerPrecedence: Int): List[Exp[RESOLV]] = {
		terms match {
			case Nil => error("terms should never be empty")

			case (r:ResolvedRef)::ts if r.operator => throw new ParseError(r+" is an operator, so it must come after another expression")
			
			case _::Nil => terms // Already reduced to the minimum

			case t::(op:ResolvedRef)::rest if op.postfix =>
				orderEvaluation(Apply(op, t)::rest)
			
			case a::(op:ResolvedRef)::rest if op.infix => {
				if (rest == Nil) throw new ParseError(op+" is an infix operator, so must have another expression after it")
				val prec = Precedence.of(op.ident)
				assert(prec != Int.MinValue) // Otherwise we might not consume all of the expression.
				if (prec > outerPrecedence){
					val (b::restofrest) = orderEvaluation(rest)(prec)
					orderEvaluation(Apply(op, TupleExp(a, b))::restofrest)
					
				}else{
					terms
				}
			}

			case f::rest => {
				val (args::restofrest) = orderEvaluation(rest)(Precedence.ofFunctionApplication)
				orderEvaluation(Apply(f, args)::restofrest)
			}
		}
	}
}

