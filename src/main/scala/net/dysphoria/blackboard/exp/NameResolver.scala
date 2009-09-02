/*
 * NameResolvers.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.collection.mutable
import Ast._
import Annotations._

object NameResolver extends CompilationPhase[Ast.Node, Ast.Node] {

	def process(in: Ast.Node, env: Environment) = {
		val (result, errors) = resolve(in, env)
		if (errors.isEmpty) Right(result) else Left(errors)
	}

	def resolve(tree: Ast.Node, env: Environment) = {
		val parseErrors = new mutable.ArrayBuffer[ParseError]

		def collectSymbols[T <: Identifier](definitionList: Iterable[T]) = {
			val symbols = new mutable.HashMap[String, T]
			for(d <- definitionList){
				if (symbols.contains(d.name))
					parseErrors += new ParseError(d, "Name defined twice: “"+d.name+"”")
				else
					symbols(d.name) = d
			}
			symbols
		}

		def resolveRec(branch: Ast.Node,
					   valueSymbols: Map[String, ValueIdentifier], 
					   typeSymbols: Map[String, TypeIdentifier], 
					   childNodesOnly: Boolean): Ast.Node = {

			def getValueSymbol(name: String) = {
				valueSymbols.get(name).orElse(
					if (env.values.isDefinedAt(name))
						Some(env.values(name))
					else
						None)
			}

			def getTypeSymbol(name: String): Option[TypeIdentifier] = {
				typeSymbols.get(name).orElse(
					if (env.types.isDefinedAt(name))
						Some(env.types(name))
					else
						None)
			}

			val mapFunction: PartialFunction[AstNode, AstNode] = {
				case s: Scope => {
					val traitSymbols = for(TraitDef(_, _, _, fns) <- s.typeIds;
										   fn<-fns) yield fn
					val innerSymbols = collectSymbols(s.ids ++ traitSymbols)
					val innerTypeSymbols = collectSymbols(s.typeIds)
					resolveRec(s, 
							   valueSymbols++ innerSymbols,
							   typeSymbols ++ innerTypeSymbols,
							   true)
				}

				case tr @ TraitDef(_, variable, _, _) => {
					resolveRec(tr,
							   valueSymbols,
							   typeSymbols + (variable.name->variable),
							   true)
				}

				case r @ ValueRef(name) =>
					getValueSymbol(name) match {
						case Some(f: FunctionLikeDecl) if f.operator => r.where(
							ResolvedRef -> f.identity,
							OperatorInfo-> OperatorInfo(f.fixity, f.precedence))
						case Some(d) => r.where(
							ResolvedRef -> d.identity,
							OperatorInfo-> NoOperatorInfo)
						case None => {
							parseErrors += new ParseError(r, "No such name "+name)
							r
						}
					}

				// A type-like-ref can only exist in a type expression, so we may
				// safely turn it into another kind of type expression: a resolved ref.
				case t @ TypeLikeRef(name, args) =>
					getTypeSymbol(name) match {
						case Some(TypeDef(_, r:ResolvedType))=>
							r // If points directly to a resolved type, substitute it directly.

						case Some(tid:TypeIdentifier) =>
							t.where(ResolvedRef -> tid.identity,
									RefIsTrait -> tid.isInstanceOf[TraitDef])

						case None =>
							parseErrors += new ParseError(t, "No such type "+name)
							t
					}

				case tr @ TraitRef(name) =>
					getTypeSymbol(name) match {
						case None =>
							parseErrors += new ParseError(tr, "No such trait "+name)
							tr
							
						case Some(d: TraitDef) => tr where (ResolvedRef -> d.identity)

						case Some(tid @ TypeDef(_, texp)) =>
							parseErrors += new ParseError(tr, "Should be a trait not a type: "+name)
							tr

						case Some(p: TypeParam) =>
							parseErrors += new ParseError(tr, "Should be a trait, not a type parameter: "+name)
							tr
					}

			}
			if (childNodesOnly)
				branch.gmapProperties(mapFunction).asInstanceOf[Ast.Node]
			else
				branch.gmap(mapFunction).asInstanceOf[Ast.Node]
		}

		val newTree = resolveRec(tree, Map.empty, Map.empty, false)
		(newTree, parseErrors.toSeq)
	}
}

