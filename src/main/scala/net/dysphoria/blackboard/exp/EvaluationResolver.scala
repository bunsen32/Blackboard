/*
 * ExpressionResolver.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.collection.mutable
import Ast._
import Annotations.OperatorInfo

object EvaluationResolver {

	def resolve(tree: Ast.Node) = {
		val errors = new mutable.ArrayBuffer[LanguageError]

		def resolveRec(tree: AstNode): AstNode = {
			tree.gmap {
				case eval @ Evaluation(terms, ternaryTail) => try {
					val body = terms map (resolveRec(_).asInstanceOf[Exp])
					// Should derive a list containing a single element
					val (exp::Nil) = orderEvaluation(body.toList)(Int.MinValue)
					ternaryTail match {
						case None => exp
						case Some(TernaryTail(ifTrue, ifFalse)) =>
							If(exp,
							   resolveRec(ifTrue).asInstanceOf[Exp],
							   resolveRec(ifFalse).asInstanceOf[Exp])
					}
				} catch {
					case CompilationException(err) => {
						errors += err
						eval // Return the input unmodified.
					}
				}
			}
		}

		def orderEvaluation(terms: List[Exp])(implicit outerPrecedence: Int): List[Exp] = 
			terms match {
				case Nil => error("terms should never be empty")

				case (r:ValueRef)::ts if r.isOperator => throw new CompilationException(
						new ParseError(r, r+" is an operator, so it must come after another expression"))

				case _::Nil => terms // Already reduced to the minimum

				case t::(op:ValueRef)::rest if op.isPostfix =>
					orderEvaluation(Apply(op, t)::rest)

				case a::(op:ValueRef)::rest if op.isInfix => {
					if (rest == Nil) throw CompilationException(
						new ParseError(op, op+" is an infix operator, so must have another expression after it"))
					val prec = (op*OperatorInfo).precedence
					assert(prec != Int.MinValue) // Otherwise we might not consume all of the expression.
					if (prec > outerPrecedence){
						val (b::restofrest) = orderEvaluation(rest)(prec)
						orderEvaluation(Apply(op, Tuple(List(a, b)))::restofrest)

					}else{
						terms
					}
				}

				case f::rest => {
					val (args::restofrest) = orderEvaluation(rest)(Precedence.ofFunctionApplication)
					orderEvaluation(Apply(f, args)::restofrest)
				}
			}


		(resolveRec(tree).asInstanceOf[Ast.Node], errors)
	}

}
