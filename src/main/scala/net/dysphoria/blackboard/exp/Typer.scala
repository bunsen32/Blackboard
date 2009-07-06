/*
 * Typer.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import blackboard.data.types._

object Typer {
	type Expr = Exp[RESOLV]

	def resolveTypes(exp: Exp[RESOLV]) {

	}

	def analyse(ast: Expr): Type = prune(analyse(ast, Set.empty))
	def analyse(ast: Expr, nongen: Set[Variable]): Type = (ast: @unchecked) match {
		case Const(v, t) => t
		case ResolvedRef(ident) => fresh(ident.typ, nongen)
		case Apply(fn, arg) => {
			val funtype = analyse(fn, nongen)
			val argtype = analyse(arg, nongen)
			val resulttype = newVariable
			unify(new Function(argtype, resulttype), funtype)
			resulttype
		}
		case Lambda(body, args @ _*) => {
			val argtype = functionArgType(args)
			val resulttype = analyse(body,
									 nongen ++ variablesIn(args.map(_.typ)))
			new Function(argtype, resulttype)
		}
		case Scope(body, defs @ _*) => {
			for(d<-defs){
				val defntype = analyse(d.resolvedExp, nongen ++ variablesIn(d.typ))
				unify(d.typ, defntype)
			}
			analyse(body, nongen)
		}
		case If(pred, ifTrue, ifFalse) => {
			val predtype = analyse(pred, nongen)
			unify(predtype, Boolean)
			val aType = analyse(ifTrue, nongen)
			val bType = analyse(ifFalse, nongen)
			unify(aType, bType)
			aType
		}
		case TupleExp(args @_*) => {
			new Tuple(for(a<-args) yield analyse(a, nongen))
		}
	}

	def functionArgType(parms: Seq[Param]): Type = parms.lengthCompare(1) match {
		case x if x<0 => Unit
		case x if x==0=> parms(0).typ
		case x if x>0 => new Tuple(parms.map(_.typ))
	}

	def variablesIn(t: Type): Iterable[Variable] = t match {
		case v: Variable => Array(v)
		case Constr(_, args)=> variablesIn(args)
	}
	def variablesIn(vs: Iterable[Type]): Iterable[Variable] = {
		vs.flatMap(variablesIn(_))
	}

	def newVariable = new Variable

	def fresh(t: Type, nongen: Set[Variable]) = {
		import scala.collection.mutable
		val mappings = new mutable.HashMap[Variable, Variable]
		def freshrec(tp: Type): Type = {
			prune(tp) match {
				case v: Variable =>
					if (isgeneric(v, nongen))
						mappings.getOrElseUpdate(v, newVariable)
					else
						v

				case Constr(name, args) =>
					Constr(name, args.map(freshrec(_)))
			}
		}

		freshrec(t)
	}



	def unify(t1: Type, t2: Type) {
		(prune(t1), prune(t2)) match {
			case (a, b) if a eq b => // Do nothing; already unified

			case (a: Variable, b) => {
				assert(a != b) // Should be ensured by first case, above + rules of Variable equality.
				if (occursintype(a, b))
					throw new TypeError("Recursive unification: "+a.toString+" cannot equal "+b.toString)
				a.instance = Some(b)
			}

			case (a: Constr, b: Variable) => unify(b, a)

			case (a: Constr, b: Constr) => {
				if (a.name != b.name ||
					a.args.length != b.args.length) throw new TypeError("Type mismatch: "+a.toString+"≠"+b.toString)

				for(i <- 0 until a.args.length)
					unify(a.args(i), b.args(i))
			}
		}
	}


	// Returns the currently defining instance of t.
	// As a side effect, collapses the list of type instances.
	def prune(t: Type): Type = t match {
		case v: Variable if v.instance.isDefined => {
			var inst = prune(v.instance.get)
			v.instance = Some(inst)
			inst
		}
		case _ => t
	}

	// Note: must be called with v 'pre-pruned'
	def isgeneric(v: Variable, nongen: Set[Variable]) = !(occursin(v, nongen))

	// Note: must be called with v 'pre-pruned'
	def occursintype(v: Variable, type2: Type): Boolean = {
		prune(type2) match {
			case `v` => true
			case Constr(name, args) => occursin(v, args)
			case _ => false
		}
	}

	def occursin(t: Variable, list: Iterable[Type]) =
		list exists (t2 => occursintype(t, t2))

}
