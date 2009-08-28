/*
 * TraitResolver.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.collection.mutable
import scala.util.Sorting
import scala.collection.Map
import Ast._
import Annotations._
import blackboard.data.{types=>t}

object TraitResolver {

	def resolve(ast: Ast.Node, env: Environment): Either[Seq[LanguageError], Map[Identity, t.Trait]] = {
		// Walks the tree, gathers all trait definitions, then orders them by
		// supertraits, (throwing error if there are any mutually recursive definitions).
		// TODO: modify to be able to look up trait defs in the Environment

		val traitSupers = new mutable.HashMap[Identity, Set[Identity]]
		val traitDefs = new mutable.HashMap[Identity, TraitDef]
		val errors = new mutable.ArrayBuffer[LanguageError]

		def recursionError(trId: Identity, superIds: Set[Identity]): Option[List[Identity]] = {
			if (superIds contains trId)
				Some(List(trId))
			else {
				for(sup <- superIds) {
					traitSupers.get(sup).map(ids =>
						recursionError(trId, ids) match {
							case None => // Do nothing
							case Some(err) => return Some(sup :: err)
						})
				}
				return None
			}
		}

		for(n <- ast) n match {
			case tr @ TraitDef(name, _, supertraits, _) =>
				val superIds = supertraits map (_*ResolvedRef)
				val superIdSet = Set(superIds: _*)
				if (superIds.size != superIdSet.size)
					errors += new ParseError(tr, "Duplicate supertraits in trait")
					
				val id = tr.identity
				traitDefs += (id -> tr)
				recursionError(id, superIdSet) match {
					case None =>
						traitSupers += (id -> superIdSet)
						
					case Some(err) =>
						// TODO: modify this to deal with traitDefs obtained from the
						// environment.
						errors += new RecursiveTraitError(tr, tr :: (err map traitDefs))
				}

			case _ => //ignore
		}

		if (errors.length == 0) {
			val result = new mutable.HashMap[Identity, t.Trait]
			def traitFor(id: Identity): t.Trait =
				result.getOrElseUpdate(id,
									   new t.Trait(traitDefs(id).name,
												   traitSupers(id) map traitFor))
			for(id <- traitDefs.keys){
				traitFor(id) // Ignore result. Depend on things being added to 'result' as side-effect.
			}
			Right(result)

		}else
			Left(errors)
	}
}
