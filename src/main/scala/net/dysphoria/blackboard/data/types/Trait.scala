/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.data.types

/**
 * Marker class for a 'trait'.
 */
case class Trait(name: String, supertraits: Set[Trait]) extends TypeLike {

	lazy val allSupertraits = supertraits ++ (supertraits flatMap (_.supertraits))
	lazy val allTraits = allSupertraits + this
}

