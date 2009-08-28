/*
 * Type.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.data.types

abstract class TypeLike

abstract class Type extends TypeLike {
	override def toString = TypeToString(this)

	/** Returns the currently defining instance of t.
	 *  As a side effect, collapses the list of type instances. For a concrete type,
	 *  (or an unbound type variable) returns the type itself.
	 */
	def pruned: Type
}

class Variable(val constraints: Set[TypeConstraint]) extends Type {
	def this() = this(Set.empty)
	var instance: Option[Type] = None
	def pruned = instance match {
		case None => this
		case Some(other) => {
			instance = Some(other.pruned)
			other
		}
	}
}

case class Constr(name: String, args: Seq[Type]) extends Type {
	def pruned = this
}

class InfixConstr(arg1: Type, name: String, arg2: Type) extends Constr(name, Array(arg1, arg2)) 

// Named with a keyword… so cannot conflict with user-defined types:
class Function(val arg: Type, val res: Type) extends InfixConstr(arg, "→", res)

// Named with a keyword… so cannot conflict with user-defined types:
class Tuple(args: Seq[Type]) extends Constr(",", args)

class Monomorphic(name: String) extends Constr(name, Nil) 

object core {
	object Unit extends Monomorphic("Unit")
	object Boolean extends Monomorphic("Boolean")
	object Int extends Monomorphic("Int")
	object Rational extends Monomorphic("Rational")
	object Real extends Monomorphic("Real")
	object Complex extends Monomorphic("Complex")
	object String extends Monomorphic("String")
}

object TypeToString {
	def apply(t: Type) = {
		import scala.collection.mutable
		val names = new mutable.HashMap[Variable, Int]
		def variableName(v: Variable) = intToGreek(names.getOrElseUpdate(v, names.size))
		def applyRec(t: Type, bracketIfInfix: Boolean): String = t.pruned match {
			case v: Variable => variableName(v)
			case Constr("→", args)=>possiblyBracket(applyRec(args(0), true)+" → "+applyRec(args(1), false), bracketIfInfix)
			case Constr(",", args)=>mkString(args, "(", ", ", ")")
			case Constr(name, args) if args.isEmpty => name
			case Constr(name, args) => name + mkString(args, "[", ", ", "]")
		}
		def mkString(types: Seq[Type], st:String, sep:String, end:String) = {
			val s = new StringBuilder(st)
			for(t<-types){
				if (s.length != st.length) s.append(sep)
				s.append(applyRec(t, false))
			}
			s.append(end).toString
		}
		def constraintString(v: Variable) = (
			for(c <- v.constraints)
				yield c match {
					case ConformsToTrait(t) => " : "+t.name
					case SubtypeOf(other)=> " <: " + applyRec(other, false)
					case SupertypeOf(other)=> " >: "+applyRec(other, false)
				}
			).mkString

		val raw = applyRec(t, false)
		if (names.keys.forall(_.constraints.isEmpty)){
			raw

		}else {
			raw + " where " + (
			for((v, n) <- names if (!v.constraints.isEmpty))
				yield variableName(v)+constraintString(v)
			).mkString(",")
		}
	}

	private val greek = "αβγδεζηθικλμνξοπρστυφχψω"
	private def intToGreek(n: Int): String = {
		val letter = n % 24
		val primes = n / 24
		return ('α'.toInt + letter).toChar.toString + (primes match {
			case 0 => ""
			case 1 => "′"
			case 2 => "″"
			case 3 => "‴"
			case 4 => "⁗"
			case _ => primes.toString
		})
	}

	private def possiblyBracket(s: String, yes: Boolean) =
		if (yes) "("+s+")" else s

}

