/*
 * TypeConstraint.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data.types


abstract class TypeConstraint{
	type Self <: TypeConstraint
	def map(f: Type => Type): Self
}
case class ConformsToTrait(other: Trait) extends TypeConstraint{
	type Self = ConformsToTrait
	def map(f: Type => Type) = this //TODO probably want to map through type parameters
}
case class SubtypeOf(other: Type) extends TypeConstraint {
	type Self = SubtypeOf
	def map(f: Type => Type) = SubtypeOf(f(other))
}
case class SupertypeOf(other: Type) extends TypeConstraint {
	type Self = SupertypeOf
	def map(f: Type => Type) = SupertypeOf(f(other))
}
