/*
 * AstNode.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.reflect.Manifest

abstract class AstNode {
	type thisType <: AstNode
	def properties: Map[Property[_], Any]
	def factory: NodeFactory[thisType]
	def apply[P](p: Property[P]): P = properties.apply(p).asInstanceOf[P]
	def *[P](p: Property[P]): P = apply(p)
	def ->[P](p: Property[P]): P = apply(p)
	def get[P](p: Property[P]): Option[P] = properties.get(p).asInstanceOf[Option[P]]

	def where[PT](p: Property[PT], v: PT) =
		factory.apply(properties.update(p, v))
	def where(pairs: Pair[Property[_], Any]*) =
		factory.apply(properties ++ pairs)

	def foreach(f: (AstNode)=>Unit) {
		f(this) // preorder traversal
		for((prop, value) <- properties){
			prop.foreach(value, f)
		}
	}

	def gmap(op: PartialFunction[AstNode, AstNode]): AstNode =
		factory.gmap(this.asInstanceOf[thisType], op)

	def gmapProperties(op: PartialFunction[AstNode, AstNode]): thisType =
		factory.gmapProperties(this.asInstanceOf[thisType], op)

	def gmap[P](p: Property[P], op: PartialFunction[AstNode, AstNode]): thisType =
		where(p, p.gmap(this*p, op))
}

abstract class NodeFactory[T<:AstNode](implicit m: Manifest[T]) { fact =>
	val manifest = m
	def apply(properties: Map[Property[_], Any]): T


	def gmap(in: T, op: PartialFunction[AstNode, AstNode]): AstNode =
		if (op.isDefinedAt(in))
			op(in)
		else
			gmapProperties(in, op)

	/**
	 * Maps node <var>node</var> to another node of the same type, but with the
	 * mapping function <var>op</var> having applied to any relevant child nodes.
	 */
	def gmapProperties(node: T, op: PartialFunction[AstNode, AstNode]): T = {
		def mapProp(pv: Pair[Property[_], Any]) :Pair[Property[_], Any] = {
			val prop = pv._1.asInstanceOf[Property[Any]]
			val mappedV = prop.gmap(pv._2, op)
			if (mappedV == pv._2)
				pv // If same after mapping, return the original
			else 
				(prop, mappedV) // Otherwise create new pair.
		}
		
		val mapped = (node.properties map (mapProp(_)))
		fact.apply(Map.empty ++ mapped)
	}
}
abstract class Property[T] {
	def ->(v: T) = (this, v)
	def pair(v: T) = (this, v)
	//def →(v: T) = (this, v) // TODO: uncomment when NetBeans starts liking Unicode.
	def gmap(v: T, op: PartialFunction[AstNode, AstNode]): T = v
	def foreach(v: T, f: (AstNode)=>Unit) {}
}

abstract class NodeProperty[E<:AstNode, T](implicit m: Manifest[E]) extends Property[T] {
	type elementType = E
	val elementTypeManifest: Manifest[elementType] = m

	/**
	 * Takes a partial function, <var>op</var>, and uses it to transform the nodes
	 * in this node property or, if not applicable to a node directly, that node's
	 * properties… and so on, recursively.
	 *
	 * If the partial function claims to be defined for a particular node, it is
	 * used to tranform the node. The runtime type of its return type must be
	 * assignable to this node property, otherwise a runtime exception will occur.
	 */
	override def gmap(v: T, op: PartialFunction[AstNode, AstNode]): T = {
		map(v, (n => {
			val transformed = n.gmap(op)
			// Assert that the thing returned by 'op' is of the correct type.
			assert(elementTypeManifest.erasure.isInstance(transformed))
			transformed.asInstanceOf[elementType]
		}))
	}
	def map(v: T, op: Function[elementType, elementType]): T
}

abstract class Property0[E<:AstNode](implicit m: Manifest[E]) extends NodeProperty[E, Option[E]]{
	def map(v: Option[E], op: Function[E, E]) = v map op
	override def foreach(v: Option[E], f: (AstNode)=>Unit) = for(n <- v) n.foreach(f)
}
abstract class Property1[E<:AstNode](implicit m: Manifest[E]) extends NodeProperty[E, E]{
	def map(v: E, op: Function[E, E]) = op(v)
	override def foreach(n: E, f: (AstNode)=>Unit) = n.foreach(f)
}
abstract class PropertyN[E<:AstNode](implicit m: Manifest[E]) extends NodeProperty[E, Seq[E]] {
	def map(v: Seq[E], op: Function[E, E]) = v map op
	override def foreach(v: Seq[E], f: (AstNode)=>Unit) = for(n <- v) n.foreach(f)
}

