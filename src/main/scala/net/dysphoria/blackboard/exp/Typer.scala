/*
 * Typer.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.collection.Map
import scala.collection.mutable
import blackboard.data.{types=>t}
import t.{core=>c}
import Ast._
import Annotations._

object Typer {

	private abstract class TypeException extends Exception
	private case class RecursiveUnificationException(a: t.Type, b: t.Type) extends TypeException
	private case class TypeMismatchException(a: t.Type, b: t.Type) extends TypeException


	/**
	 * The non-generic type variables and in-scope type behaviours
	 * which affect type type-checker at the current scope being type-checked.
	 */
	private case class TypingScope(nongen: Set[t.Variable], behaviours: t.Behaviour) {
		def addV(newNongen: Iterable[t.Variable]) = TypingScope(nongen ++ newNongen, behaviours)
		def addT(newTraitInstances: Iterable[TraitInstance])(implicit map:IdentifierMap) = {
			val newBeh = (behaviours /: newTraitInstances)((b, t) =>
				b.withTreatAs(typeOf(t.treat), t.declaredTraits map (t => traitForId(t*ResolvedRef))))
			TypingScope(nongen, newBeh)
		}
		def addC(tSub: t.Type, tSup: t.Type) = 
			TypingScope(nongen, behaviours.withSubtype(tSub, tSup))
	}
	private val rootTypingScope = TypingScope(Set.empty, t.Behaviour.empty)

	/**
	 * Data structure which allows typelikeOfId to locate the t.Type associated with
	 * a particular Identity (where an Identity is the unique identifier of a syntax tree
	 * node). These may come from the 'Environment' (precompiled or built-in identifiers)
	 * or from the module currently being compiled.
	 */
	private case class IdentifierMap(env: Environment, 
									 idsToTypes: mutable.Map[Identity, t.Type],
									 idsToTraits: Map[Identity, t.Trait])


	def typeOf(n: Ast.Node)(implicit map: IdentifierMap): t.Type =
		typeOfId(n.identity)
		
	def typeOfId(id: Identity)(implicit map: IdentifierMap): t.Type =
		if (map.idsToTraits.contains(id))
			error(id+" is a trait, not a type.")
		else if (map.env.ids.contains(id))
			(map.env.ids(id)*Type).pruned
		else
			map.idsToTypes.getOrElseUpdate(id, new t.Variable).pruned

	def traitForId(id: Identity)(implicit map: IdentifierMap): t.Trait =
		map.idsToTraits.getOrElse(id, error("Trait not found: "+id))

	def functionArgType(parms: Seq[Param])(implicit map: IdentifierMap): t.Type = parms.lengthCompare(1) match {
		case x if x<0 => c.Unit
		case x if x==0=> typeOf(parms(0))
		case x if x>0 => new t.Tuple(parms.map(typeOf))
	}


	/**
	 * Initial typing phase to <ol>
	 *   <li>interpret explicit type expressions
	 *   <li>assign a type variable to each node in the tree</li>
	 * </ol>
	 * (Merging the type of a definition with its value is done at type-check time
	 * (because among other reasons, that depends on access to the current type
	 * behaviour context.))
	 * Returns a pair of (new-tree, type-errors).
	 */
	def resolve(tree: Ast.Node, traits: Map[Identity, t.Trait], env: Environment) = {
		val typeErrors = new mutable.ArrayBuffer[TypeError]
		val idsToTypes = new mutable.HashMap[Identity, t.Type]
		implicit val map = IdentifierMap(env, idsToTypes, traits)

		def logTypeError(ast: Ast.Node, ex: TypeException) =
			typeErrors.append(ex match {
				case TypeMismatchException(a, b) => new TypeMismatchError(ast, a, b)
				case RecursiveUnificationException(a, b) => new RecursiveTypeError(ast, a, b)
			})

		/**
		 * Adds a Type annotation to node 'n' and returns the new node, and adds
		 * ('n'.identity -> 'typ') to the 'idsToTypes' map as a side-effect.
		 */
		def typedNode[T<:Ast.Node](n: T, typ: t.Type)(implicit x: TypingScope): T = {
			idsToTypes.get(n.identity) match {
				case Some(existing: t.Type) => unify(typ, existing)
				case None => idsToTypes(n.identity) = typ
			}
			(n where (Type -> typ.pruned)).asInstanceOf[T]
		}

		/**
		 * Given an expression, returns an expression with a type annotation.
		 */
		def analyseRec(ast: Exp)(implicit x: TypingScope): Exp = {
			ast match {
				case l @ Lambda(body, args, tparams) => {
					val nestedScope = x addV variablesIn(args.map(typeOf(_).pruned))
					val resolvedBody = resolveRec(body, false)(nestedScope)
					val resolved = l where (
						// TODO: Deal with type parameters
						Lambda.Params -> (for(p<-args) yield resolveRec(p, false)),
						Lambda.Body -> resolvedBody)
					typedNode(resolved, new t.Function(functionArgType(args), typeOf(body)))
				}
				case s @ Block(_, _, _, _) => {
					val (impl, expl, nestedScope) = resolveDefs(s.defs)(x addT s.traitInstances)
					val res = s where (
						Block.TypeDefs -> (for(d<-s.typeDefs) yield resolveRec(d, false)(nestedScope)),
						Block.TraitInstances -> (for(b<-s.traitInstances) yield resolveRec(b, false)(nestedScope)),
						Block.Defs -> (impl ++ expl),
						Block.Body -> analyseRec(s.body)(nestedScope))
		  
					typedNode(res, typeOf(s.body))
				}
				case s @ Module(_, _, _) => {
					val (impl, expl, nestedScope) = resolveDefs(s.defs)(x addT s.traitInstances)
					val res = s where (
						Module.TypeDefs -> (for(d<-s.typeDefs) yield resolveRec(d, false)(nestedScope)),
						Module.TraitInstances -> (for(b<-s.traitInstances) yield resolveRec(b, false)(nestedScope)),
						Module.Defs -> (impl ++ expl))
					typedNode(res, c.Unit)
				}
				
				case _ => {
					val node = resolveRec(ast, true) 
					val typ = node match {
						case Const(_, t) => t
						case r: ValueRef => fresh(typeOfId(r*ResolvedRef), x.nongen)
						case Apply(fn, arg) => {
							unify(new t.Function(typeOf(arg), typeOf(node)), typeOf(fn))
							typeOf(node)
						}
						case If(pred, ifTrue, ifFalse) => {
							unify(typeOf(pred), c.Boolean)
							unify(typeOf(ifTrue), typeOf(ifFalse))
						}
						case Tuple(args) => {
							new t.Tuple(for(a<-args) yield typeOf(a))
						}
						case ExplicitType(tExp, inner) => {
							val explicit = interpretTypeExp(tExp)
							unify(explicit, typeOf(inner))
						}
						case _ => error("Should not have attempted to type "+ast)
					}
					typedNode(node, typ)
				}
			}
		}

		def resolveDefs(defs: Iterable[ValueDef])(implicit x: TypingScope) = {
			val (impl, expl) = defs.partition(_ match {
				case f: FunctionDef if f.isImplicit => println("!!!!!!"); true
				case _ => false
			})
			val (implR, newScope) = resolveImplicitDefs(impl)(x)
			val explR = resolveExplicitDefs(expl)(newScope)
			(implR.toList, explR.toList, newScope)
		}

		def resolveImplicitDefs(defs: Iterable[ValueDef])(implicit x: TypingScope): Pair[Iterable[ValueDef], TypingScope] = {
			var nextScope = x
			val resolvedDefs = for(d <- defs) yield d match {
				case f: FunctionDef if f.isImplicit =>
					val fType = typeOf(f).asInstanceOf[t.Function]
					// Ensure that fType only contains non-generic variables
					// (That is: implicit conversions cannot have their types inferred;
					// that way madness lies.) TODO: Report as a nicer error.
					assert(!containsFreeVariables(fType))
					val defScope = nextScope addV variablesIn(fType)
					nextScope = x.addC(fType.arg, fType.res)
					val r: ValueDef = resolveRec(f, false)(defScope)
					r

				case _ => error("Should only receive (implicit) FunctionDefs here")
			}
			(resolvedDefs, nextScope)
		}
		
		def resolveExplicitDefs(defs: Iterable[ValueDef])(implicit x: TypingScope): Iterable[ValueDef] =
			for(d <- defs;
				defScope = x addV variablesIn(typeOf(d)))
					yield resolveRec(d, false)(defScope)


		/**
		 * Given an arbitrary parse tree, 'tree', annotates it with types, returning
		 * the new, annotated, tree. Calls 'analyseRec' for expressions.
		 */
		def resolveRec[T<:Ast.Node](tree: T, childNodesOnly: Boolean)(implicit tScope: TypingScope): T = {
			val mapper: PartialFunction[AstNode,AstNode] = {
				case e: Exp => {
					val res = analyseRec(e)
					res
				}
				case d: ValueDef => {
					val t = interpretTypeExp(d.declaredType)
					typedNode(resolveRec(d, true), unify(t, typeOf(d.value)))
				}
				case i: ValueIdentifier => {
					val t = interpretTypeExp(i.declaredType)
					val res = typedNode(resolveRec(i, true), t)
					res
				}
				case d: TypeDef => {
					typedNode(d, interpretTypeExp(d.declaredType))
				}
			}
			try {
				if (childNodesOnly)
					tree.gmapProperties(mapper).asInstanceOf[T]
				else
					tree.gmap(mapper).asInstanceOf[T]
					
			}catch {
				case x: TypeException => {
					logTypeError(tree, x)
					tree
				}
			}
		}

		(resolveRec(tree, false)(rootTypingScope), typeErrors)
	}

	// TODO: TEST!
	def containsFreeVariables(t0: t.Type)(implicit x: TypingScope): Boolean = t0.pruned match {
		case v: t.Variable => !isgeneric(v, x.nongen)
		case t.Constr(_, args) => args.exists(containsFreeVariables)
	}

	def interpretTypeExp(texp: TypeExp)(implicit map: IdentifierMap): t.Type = texp match {
		case _: NoTypeExpClass => new t.Variable
		case _: UnitTypeExpClass => c.Unit
		case FunctionTypeExp(a, r) => new t.Function(interpretTypeExp(a),
													 interpretTypeExp(r))
		case TupleTypeExp(els) => new t.Tuple(els map interpretTypeExp)
		case ResolvedType(t) => t
		case TypeLikeRef(_, a) => {
			// Should never get an externally-defined type here, since such
			// references should have been converted to ResolvedTypes already.
			// TODO: Need to deal with type arguments
			assert(a.isEmpty, "Cannot currently deal with type arguments")
			if (texp*RefIsTrait) {
				new t.Variable(Set(t.ConformsToTrait(traitForId(texp*ResolvedRef))))
			}else{
				typeOfId(texp*ResolvedRef)
			}
		}
	}


	def variablesIn(t0: t.Type): Iterable[t.Variable] = t0 match {
		case v: t.Variable => Some(v)
		case t.Constr(_, args)=> variablesIn(args)
	}
	def variablesIn(vs: Iterable[t.Type]): Iterable[t.Variable] = {
		vs.flatMap(variablesIn(_))
	}

	def fresh(t0: t.Type, nongen: Set[t.Variable]) = {
		import scala.collection.mutable
		val mappings = new mutable.HashMap[t.Variable, t.Variable]
		def freshrec(tp: t.Type): t.Type = {
			tp.pruned match {
				case v: t.Variable =>
					if (isgeneric(v, nongen))
						mappings.getOrElseUpdate(v, new t.Variable(v.constraints))
					else
						v

				case t.Constr(name, args) =>
					t.Constr(name, args.map(freshrec(_)))
			}
		}

		freshrec(t0)
	}



	def unify(t1: t.Type, t2: t.Type)(implicit tScope: TypingScope): t.Type = {
		(t1.pruned, t2.pruned) match {
			case (a, b) if a eq b => // Do nothing; already unified

			case (a: t.Variable, b: t.Variable) => {
				assert(a != b) // Should be ensured by first case, above + rules of Variable equality.
				if (a.constraints.isEmpty)
					a.instance = Some(b)

				else if (b.constraints.isEmpty)
					b.instance = Some(a)
					
				else {
					val c = new t.Variable(mergeConstraints(a.constraints, b.constraints))
					a.instance = Some(c)
					b.instance = Some(c)
				}
			}

			case (a: t.Variable, b: t.Constr) => {
				assert(a != b)
				if (occursintype(a, b)) throw new RecursiveUnificationException(a, b)
				checkConstraints(a.constraints, b)
				a.instance = Some(b)
			}

			// Same as (variable, constr) case. Avoid duplicated code-- just swap:
			case (a: t.Constr, b: t.Variable) => unify(b, a)

			case (a: t.Constr, b: t.Constr) => {
				if (a.name != b.name ||
					a.args.length != b.args.length) throw new TypeMismatchException(a, b)

				for(i <- 0 until a.args.length)
					unify(a.args(i), b.args(i))
			}
		}
		t1.pruned // (Prune again)
	}

	/**
	 * Given two sets of type contraints, merge them. Currently no attempt is made
	 * at this point to spot incompatable constraints (though doing so would produce
	 * potentially clearer error messages). */
	def mergeConstraints(c1: Set[t.TypeConstraint], c2: Set[t.TypeConstraint]) = {
		Set.empty[t.TypeConstraint] ++
			(for(c <- c1) yield c.map(_.pruned)) ++
			(for(c <- c2) yield c.map(_.pruned))
	}

	/**
	 * Assert that concrete type 'c' conforms to set of type constraints 'cs'.
	 * If it does not, throw a suitable subclass of TypeException.
	 */
	def checkConstraints(cs: Set[t.TypeConstraint], c: t.Constr)(implicit tScope: TypingScope) {
		for(cst <- cs) cst match {
			case t.ConformsToTrait(tr) => {
				println("Type "+c+" should implement "+tr.name)
				if (!tScope.behaviours.implements(c, tr))
					throw new TypeMismatchException(c, null) // TODO "Type "+t+" does not conform to trait "+tr.name
			}
		}
	}


	// Note: must be called with v 'pre-pruned'
	def isgeneric(v: t.Variable, nongen: Set[t.Variable]) = !(occursin(v, nongen))

	// Note: must be called with v 'pre-pruned'
	def occursintype(v: t.Variable, type2: t.Type): Boolean = {
		type2.pruned match {
			case `v` => true
			case t.Constr(name, args) => occursin(v, args)
			case _ => false
		}
	}

	def occursin(v: t.Variable, list: Iterable[t.Type]) =
		list exists (t2 => occursintype(v, t2))

}
