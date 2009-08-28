/*
 * Behaviours.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data.types

/**
 * Represents a set of type behaviours, i.e., <ol>
 * <li>Typeclasses of types which conform to traits</li>
 * <li>The set of sub/supertyping relations between types</li>
 * </ol>
 */
class Behaviour(typeclasses: Map[Type, Set[Trait]], supertypes: Map[(Type, Type), Int]) {
	private def this() = this(Map.empty, Map.empty)

	/**
	 * Returns a new Behaviour in which Type 't' implements traits 'traits'.
	 * Note that if trait A extends B, and Type T implements A, it also, implictly,
	 * implements B.
	 */
	def withTreatAs(t: Type, traits: Iterable[Trait]): Behaviour = {
		val newTraits = Set.empty ++ traits.flatMap(_.allTraits)
		val allNewTraits = typeclasses(t) ++ newTraits
		new Behaviour(typeclasses.update(t, allNewTraits), supertypes)
	}

	/**
	 * Returns a modified Behaviour with a new subtyping relation between 'sub' and 'sup'.
	 * This will override any existing subtype relations which have the same or greater
	 * 'cost'.
	 * The sub-supertype relation graph does not allow any cycles (i.e., it is a Directed
	 * Acyclic Graph). The number of edges between any given sub and supertype is the
	 * 'cost' associated with that type conversion.
	 * Note that, as a runtime efficiency, the graph stores all paths, even indirect ones
	 * (like A->B->C) explicitly (instead of implicitly, as {A->B, B->C}).
	 */
	def withSubtype(sub: Type, sup: Type): Behaviour = {
		// We're adding a strict subtyping relation.
		require(sub != sup)
		// Fail if the supposed supertype is actually an existing subtype.
		require(! isSubtypeOf(sup, sub))
		// Fail if there already exists a length-1 path from 'sub' to 'sup'
		require(supertypes.get((sub, sup)) != Some(1))

		def lowerCost(existing: Map[(Type, Type), Int], sub: Type, sup: Type, cost: Int) =
			cost <= existing.getOrElse((sub, sup), Int.MaxValue)

		if (! lowerCost(supertypes, sub, sup, 1))
			this // Return existing behaviour.
		else {
			// Add this direct sub-super-type relation
			var relation = ((sub, sup) -> 1)
			// Add all the implied such relations where we have existing subtype relations
			// for the subtype being added:
			var s2 = (for(r@((x, `sub`), c) <- supertypes;
						  totalCost = c + 1;
						  if lowerCost(supertypes, x, sup, totalCost))
								yield ((x, sup), totalCost)) ++ Seq(relation)
			assert(s2.size > 0)
			// Add all the implied such relations where we have existing SUPERtype relations
			// for the SUPERtype being added:
			var s3 = (for(((x, _), c1) <- s2;
						  ((`sup`, y), c2) <- supertypes;
						  totalCost = c1 + c2;
						  if lowerCost(supertypes, x, y, totalCost))
								yield ((x, y), totalCost))

			new Behaviour(typeclasses, supertypes ++ s2 ++ s3)
		}
	}

	/**
	 * Returns true iff type 't' implements the trait 'tr'.
	 */
	def implements(t: Type, tr: Trait): Boolean =
		typeclasses.getOrElse(t, Set.empty) contains tr
	
	/**
	 * Returns true iff type 'sub' is a subtype of (has a series of conversions to
	 * type 'sup'. Does not care if there is no unique shortest conversion.
	 */
	def isSubtypeOf(sub: Type, sup: Type): Boolean =
		supertypes.contains((sub, sup))
	
	/**
	 * Returns the path length of conversions from subtype 'sub' to supertype
	 * 'sup'. If there is no unique shortest path, throws an exception.
	 */
	def conversionTo(sub: Type, sup: Type): Int =
		supertypes((sub, sup))

}

object Behaviour {
	val empty = new Behaviour
}

