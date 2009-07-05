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
		case Evaluation(terms) => resolveEvaluation(terms)
		case TupleExp(terms @ _*) => TupleExp((terms map (resolve(_))): _*)
		case c: Const[_] => c
		case Dereference(name) => ResolvedRef(env.defs(name))
		case TernaryTail(ifTrue, ifFalse) => TernaryTail(resolve(ifTrue), resolve(ifFalse))
		case _: Apply => error("Should not have any 'Apply's before RESOLV phase")
		case _: ResolvedRef => error("Should not have any 'ResolvedRef's before RESOLV phase")
	}

	def resolve(lambda: Lambda[PARSE])(implicit outer: Env): Lambda[RESOLV] = {
		resolveScope[Lambda[PARSE]](lambda, (l, env) => Lambda(resolve(l.body)(env), l.params: _*))
	}

	def resolve(scope: Scope[PARSE])(implicit outer: Env): Scope[RESOLV] = {
		resolveScope[Scope[PARSE]](scope, (s, env) => {
			val newS = Scope(resolve(s.body)(env), s.defn: _*)
			for(d <- newS.defn){
				d.resolvedExp = resolve(d.parsedExp)
			}
			newS
		})
	}

	def resolveScope[S<:ScopedExp[PARSE]](scope: S, generator: (S,Env)=>S#ScopeType[RESOLV])(implicit outer: Env): S#ScopeType[RESOLV] = {
		implicit val env = new NestedEnv(outer)
		for(d <- scope.ids) {
			val name = d.name
			if (env.localDefs.contains(name))
				throw new Exception("Duplicate definition of "+name)
			else
				env.localDefs += (name -> d)
		}

		generator(scope, env)
	}


	def resolveEvaluation(unresolvedTerms: List[Exp[PARSE]])(implicit env: Env): Exp[RESOLV] = {
		val terms = for(val u <- unresolvedTerms) yield resolve(u)
		orderEvaluation(terms)(Int.MinValue) match {
			case exp::Nil => exp
			case _ => throw new Exception("THAT shouldn't happen.")
		}
	}

	def orderEvaluation(terms: List[Exp[RESOLV]])(implicit outerPrecedence: Int): List[Exp[RESOLV]] = {
		terms match {
			case Nil => error("terms should never be empty")

			case (r:ResolvedRef)::ts if r.operator => throw new Exception(r+" is an operator, so it must come after another expression")
			
			case _::Nil => terms // Already reduced to the minimum

			case t::(op:ResolvedRef)::rest if op.postfix =>
				orderEvaluation(Apply(op, t)::rest)
			
			case a::(op:ResolvedRef)::rest if op.infix => {
				if (rest == Nil) throw new Exception(op+" is an infix operator, so must have another expression after it")
				val prec = Precedence.of(op.ident)
				assert(prec != Int.MinValue) // Otherwise we might not consume all of the expression.
				if (prec > outerPrecedence){
					val (b::restofrest) = orderEvaluation(rest)(prec)
					orderEvaluation(Apply(op, TupleExp(a, b))::restofrest)
					
				}else{
					terms
				}
			}

			case p::(tern:TernaryTail[RESOLV])::rest => {
				assume(rest == Nil, "Should be nothing after a ternary-tail")
				val prec = Precedence.ofTernaryOperator
				if (prec > outerPrecedence){
					If(p, tern.ifTrue, tern.ifFalse)::Nil

				}else {
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

