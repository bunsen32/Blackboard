/*
 * Trait.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data.types

/**
 * Marker class for a 'trait'.
 */
case class Trait(name: String, supertraits: Set[Trait]) extends TypeLike {

	lazy val allSupertraits = supertraits ++ (supertraits flatMap (_.supertraits))
	lazy val allTraits = allSupertraits + this
}

