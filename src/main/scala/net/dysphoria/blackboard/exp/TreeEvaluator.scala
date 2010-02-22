/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
/*
package net.dysphoria.blackboard.exp

import scala.collection.mutable
import blackboard.data.types
import TypedSyntax._

/**
 * Evaluates expressions (Expr instances) directly. Not the most efficient way to do it,
 * but simple in terms of implementation.
 */
object TreeEvaluator {
	type Expr = Exp

	def eval(exp: Expr): Any = evalRec(exp, GlobalFrame)
	def evalRec(exp: Expr, frame: StackFrame): Any = (exp: @unchecked) match {
		case Const(v, _) => v

		case Reference(id) => frame.deref(id)

		case s: Scope => {
			evalRec(s.body, new ScopeFrame(frame, s))
		}

		case If(pred, trueExp, falseExp) =>
			if(evalRec(pred, frame) == true)
				evalRec(trueExp, frame)
			else
				evalRec(falseExp, frame)

		case t: Tuple => {
			new Tup((for(term <- t.terms) yield new IndirectThunk(term, frame)).toArray)
		}

		case l: Lambda => {
			l.params.lengthCompare(1) match {
				case x if x<0 => new Execution0(l, frame)
				case x if x==0=> new Execution1(l, frame)
				case x if x>0 => new ExecutionN(l, frame)
			}
		}

		case Apply(fn, arg) => {
			evalRec(fn, frame) match {
				case f: Execution0 => f()
				case f: Execution1 => f(frame.thunk(arg))
				case f: ExecutionN => f(frame.thunk(arg))
				case n: NativeFunction0 => n.op(NativeParams0)
				case n: NativeFunction1 => n.op(new NativeParams1(frame.thunk(arg)))
				case n: NativeFunctionN => n.op(new NativeParamsN(frame.thunk(arg)))
				case _ => error("Tried to evaluate a non-function")
			}
		}

		case PolyExp(exp, params) => new Polymorph(exp, frame, params)

		case InstantiatePoly(exp, typeArgs) => evalRec(exp, frame) match {
			case p: Polymorph => p(typeArgs map (eval(_, frame)))
			case other => error("Tried to InstantiatePoly a "+other)
		}

		case InstantiateTrait(typ, fnDefn) => eval(typ, frame)(fnDefn)

	}

	def eval(typ: TypeClassExp, frame: StackFrame) = typ match {
		case TypeClassLiteral(t) => t
		case TypeClassRef(p) => frame.derefType(p)
	}

	class Tup(val thunks: Seq[Thunk]){
		override def toString = {
			for(t <- thunks) {t.value; ()}
			thunks.mkString("(", ", ", ")")
		}
	}

	class Polymorph(exec: Exp, static: StackFrame, params: Seq[TypeParam]) {
		def apply(args: Seq[TypeClass]) = {
			val frame = new TypeFrame(static, params, args)
			evalRec(exec, frame)
		}
	}

	class Execution0(fun: Lambda, static: StackFrame) extends Function0[Any] {
		def apply() = evalRec(fun.body, static)
	}

	class Execution1(fun: Lambda, static: StackFrame) extends Function1[Thunk, Any] {
		def apply(arg: Thunk) = evalRec(fun.body, new CallFrame1(static, fun, arg))
	}

	class ExecutionN(fun: Lambda, static: StackFrame) extends Function1[Thunk, Any] {
		def apply(arg: Thunk) = evalRec(fun.body, new CallFrameN(static, fun, arg))
	}

	abstract class StackFrame {
		def thunk(id: Expr): Thunk
		def deref(id: VariableIdentifier): Any
		def derefType(t: TypeParam): TypeClass
	}

	object GlobalFrame extends StackFrame {
		val ids = Set.empty ++ BuiltIn.functionList

		def thunk(exp: Expr) = new IndirectThunk(exp, this)
		def deref(id: VariableIdentifier) = id match {
			case d: FunctionDef if ids contains d => d.value.asInstanceOf[Const].value
			case _ => error("Cannot find identifier "+id)
		}
		def derefType(t: TypeParam) = throw new UnsupportedOperationException("TODO");
	}

	class ScopeFrame(container: StackFrame, lexicalScope: Scope) extends StackFrame{
		val ids = Set.empty ++ (lexicalScope.defn)
		val values = new mutable.HashMap[Identifier, Any]

		def thunk(exp: Expr) = exp match {
			case i: Identifier if values.contains(i) => new ValThunk(values(i))
			case _ => new IndirectThunk(exp, this)
		}
		def deref(id: VariableIdentifier) = id match {
			case d: Defn if ids contains d => values.getOrElseUpdate(d, evalRec(d.value, this))
			case _ => container.deref(id)
		}
		def derefType(t: TypeParam) = container.derefType(t)
	}

	class TypeFrame(container: StackFrame, params: Seq[TypeParam], args: Seq[TypeClass]) extends StackFrame{
		require(params.length == args.length)
		val typeMap = Map.empty ++ (params.elements.zip(args.elements))

		def thunk(exp: Expr) = container.thunk(exp)
		def deref(id: VariableIdentifier) = container.deref(id)
		def derefType(t: TypeParam) = typeMap.getOrElse(t, container.derefType(t))
	}

	object NativeParams0 extends NativeParams {
		def apply[P](index: Int) = error("Attempt to dereference parameter "+index)
	}

	abstract class CallFrame(container: StackFrame) extends StackFrame {
		override def derefType(t: TypeParam) = container.derefType(t)
	}

	class CallFrame1(container: StackFrame, function: Lambda, arg: Thunk) extends CallFrame(container) {
		val param0 = function.params(0)

		def thunk(exp: Expr) = exp match {
			case p: Param if p == param0 => arg
			case _ => new IndirectThunk(exp, this)
		}
		def deref(id: VariableIdentifier) = id match {
			case p: Param if id == param0 => arg.value
			case _ => container.deref(id)
		}
	}

	class NativeParams1(arg: Thunk) extends NativeParams{
		def apply[P](index: Int) = {assume(index == 0); arg.value.asInstanceOf[P]}
	}

	class CallFrameN(container: StackFrame, function: Lambda, params: Thunk) extends CallFrame(container) {
		lazy val ids = Map.empty ++ (function.params.elements.zip(params.value.asInstanceOf[Tup].thunks.elements))

		def thunk(exp: Expr) = exp match {
			case p: Param if ids contains p => ids(p)
			case _ => new IndirectThunk(exp, this)
		}
		def deref(id: VariableIdentifier) = id match {
			case p: Param if ids contains p => ids(p).value
			case _ => container.deref(id)
		}
	}

	class NativeParamsN(args: Thunk) extends NativeParams {
		def apply[P](index: Int) = {
			val params = args.value.asInstanceOf[Tup].thunks

			params(index).value match {
				case t: Thunk => error(t.toString)
				case v => v.asInstanceOf[P]
			}
		}
	}

	abstract class Thunk {
		def value: Any
	}

	// No evaluation required. I thank you.
	class ValThunk(override val value: Any) extends Thunk

	// This one is a deferred execution. When asked for the result, we execute it.
	class IndirectThunk(private[this] var code: Expr, private[this] var context: StackFrame) extends Thunk {
		override lazy val value = {
			val result = evalRec(code, context)
			code = null // Un-reference these two, to allow garbage collection.
			context = null
			result
		}

		override def toString = code match {
			case null => value.toString
			case _ => "#"+code.toString
		}
	}

}


*/
