/*
 * TreeEvaluator.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.collection.mutable

/**
 * Evaluates expressions (Expr instances) directly. Not the most efficient way to do it,
 * but simple in terms of implementation.
 */
object TreeEvaluator {
	type Expr = Exp[RESOLV]

	def eval(exp: Expr): Any = evalRec(exp)(GlobalFrame)
	def evalRec(exp: Expr)(implicit frame: StackFrame): Any = exp match {
		case Const(v, _) => v

		case ResolvedRef(id) => frame.deref(id)

		case If(pred, trueExp, falseExp) =>
			if(evalRec(pred) == true) evalRec(trueExp) else evalRec(falseExp)

		case t: TupleExp[RESOLV] => {
			new Tup((for(term <- t.terms) yield new IndirectThunk(term, frame)).toArray)
		}

		case Apply(fn, arg) => {
			evalRec(fn) match {
				case f: Execution0 => f()
				case f: Execution1 => f(frame.thunk(arg))
				case f: ExecutionN => f(frame.thunk(arg))
				case n: NativeFunction0 => n.op(NativeParams0)
				case n: NativeFunction1 => n.op(new NativeParams1(frame.thunk(arg)))
				case n: NativeFunctionN => n.op(new NativeParamsN(frame.thunk(arg)))
				case _ => error("Tried to evaluate a non-function")
			}
		}

		case l: Lambda[RESOLV] => {
			l.params.lengthCompare(1) match {
				case x if x<0 => new Execution0(l, frame)
				case x if x==0=> new Execution1(l, frame)
				case x if x>0 => new ExecutionN(l, frame)
			}
		}

		case s: Scope[RESOLV] => {
			evalRec(s.body)(new ScopeFrame(frame, s))
		}
	}

	class Tup(val thunks: Seq[Thunk]){
		override def toString = {
			for(t <- thunks) {t.value; ()}
			thunks.mkString("(", ", ", ")")
		}
	}

	class Execution0(fun: Lambda[RESOLV], static: StackFrame) extends Function0[Any] {
		def apply() = evalRec(fun.body)(static)
	}

	class Execution1(fun: Lambda[RESOLV], static: StackFrame) extends Function1[Thunk, Any] {
		def apply(arg: Thunk) = evalRec(fun.body)(new CallFrame1(static, fun, arg))
	}

	class ExecutionN(fun: Lambda[RESOLV], static: StackFrame) extends Function1[Thunk, Any] {
		def apply(arg: Thunk) = evalRec(fun.body)(new CallFrameN(static, fun, arg))
	}

	abstract class StackFrame {
		def thunk(id: Expr): Thunk
		def deref(id: Identifier): Any
	}

	object GlobalFrame extends StackFrame {
		val ids = Set.empty ++ BuiltIn.localDefs.values
		for(id <- ids) {
			id.resolvedExp = NameResolver.resolve(id.parsedExp)(null)
		}

		def thunk(exp: Expr) = new IndirectThunk(exp, this)
		def deref(id: Identifier) = id match {
			case d: FunctionDefn if ids contains d => d.resolvedExp.asInstanceOf[Const].value
			case _ => error("Cannot find identifier "+id)
		}
	}

	class ScopeFrame(container: StackFrame, lexicalScope: Scope[RESOLV]) extends StackFrame{
		val ids = Set.empty ++ (lexicalScope.ids)
		val values = new mutable.HashMap[Identifier, Any]

		def thunk(exp: Expr) = exp match {
			case i: Identifier if values.contains(i) => new ValThunk(values(i))
			case _ => new IndirectThunk(exp, this)
		}
		def deref(id: Identifier) = id match {
			case d: Defn if ids contains id => values.getOrElseUpdate(id, evalRec(d.resolvedExp)(this))
			case _ => container.deref(id)
		}
	}

	object NativeParams0 extends NativeParams {
		def apply[P](index: Int) = error("Attempt to dereference parameter "+index)
	}

	class CallFrame1(container: StackFrame, function: Lambda[RESOLV], arg: Thunk) extends StackFrame {
		val param0 = function.params(0)

		def thunk(exp: Expr) = exp match {
			case p: Param if p == param0 => arg
			case _ => new IndirectThunk(exp, this)
		}
		def deref(id: Identifier) = id match {
			case p: Param if id == param0 => arg.value
			case _ => container.deref(id)
		}
	}

	class NativeParams1(arg: Thunk) extends NativeParams{
		def apply[P](index: Int) = {assume(index == 0); arg.value.asInstanceOf[P]}
	}

	class CallFrameN(container: StackFrame, function: Lambda[RESOLV], params: Thunk) extends StackFrame {
		lazy val ids = Map.empty ++ (function.params.elements.zip(params.value.asInstanceOf[Tup].thunks.elements))

		def thunk(exp: Expr) = exp match {
			case p: Param if ids contains p => ids(p)
			case _ => new IndirectThunk(exp, this)
		}
		def deref(id: Identifier) = id match {
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
			val result = evalRec(code)(context)
			code = null // Dereference these two, to allow garbage collection.
			context = null
			result
		}

		override def toString = code match {
			case null => value.toString
			case _ => "#"+code.toString
		}
	}

}

abstract class NativeParams {
	def apply[P](index: Int): P
}

abstract class NativeFunction {
	val op: NativeParams=>Any
}
case class NativeFunction0(override val op: NativeParams=>Any) extends NativeFunction
case class NativeFunction1(override val op: NativeParams=>Any) extends NativeFunction
case class NativeFunctionN(override val op: NativeParams=>Any) extends NativeFunction

