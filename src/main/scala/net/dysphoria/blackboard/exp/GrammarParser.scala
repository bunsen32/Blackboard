/*
 * Language.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.TokenParsers
import blackboard.data.types
import types.{core=>c}

import Ast._

object GrammarParser extends TokenParsers {
	type Tokens = Tokeniser.type
	val lexical = Tokeniser

	def parseDefinitions(in: Reader[Char]) = {
		val lexer = new lexical.Scanner(in)
		phrase(definitions) (lexer)
	}

	def parseExpression(in: Reader[Char]) = {
		val lexer = new lexical.Scanner(in)
		phrase(exp) (lexer)
	}

	import lexical._

	// Conversions

	implicit def acc(t: Elem): Parser[Elem] = acceptIf(_ == t)("‘"+t+"’ expected, but "+ _ +" found")
	implicit def optionTypeToTypeExp(optT: Option[TypeExp]) =
		optT.getOrElse(NoTypeExp)


	// EXPRESSIONS
	
	type ParserExp = Parser[Exp]

	def expblock: ParserExp = OpenBrace~>opt(definitions<~Semicolon)~exp<~opt(Semicolon)<~CloseBrace ^^ {
		case Some((vars, types, tBehaviours))~exp => Block(exp, vars, types, tBehaviours)
		case None~exp => exp
	}
	def exp: ParserExp = untyped_exp~opt(Colon~>typeexp) ^^ {
		case exp~typ => exp
	}
	def untyped_exp: ParserExp = term~rep(term)~opt(ternarytail) ^^ {
		case head~tail~ternary => Evaluation(head::tail, ternary)
	}
	def term: ParserExp = (
		expblock
	|	bracketed
	//|	seq_exp
	|	deref
	|	value
	|	ifthenelse
	|	anon_fn
	|	failure("Not a valid expression term"))


	def bracketed = OpenParen~>repsep(exp,Comma)<~CloseParen ^^ {
		case exps => exps.lengthCompare(1) match {
			case x if x<0 => Const((), c.Unit)
			case x if x==0=> exps(0)
			case x if x>0 => Tuple(exps)
		}
	}
	def bracketedsingleexp = OpenParen ~> exp <~ CloseParen
	def deref = accept("name reference", {
			case Ident(n) => ValueRef(n)
			case Equals => ValueRef("=") // Allow use of '=' as an operator.
		})
	def ifthenelse:Parser[Exp] = If~bracketedsingleexp~exp~Else~exp ^^
		{case If~cond~truePath~Else~falsePath => Ast.If.apply(cond, truePath, falsePath)}
	def ternarytail = Question~untyped_exp~Colon~untyped_exp ^^
		{case Question~x~Colon~y => TernaryTail(x, y)}

	def value = val_string | val_number | val_real | val_true | val_false
	def val_number = accept("number", {case DigitString(n) => Const(n.toInt, c.Int)})
	def val_real = accept("real", {case RealNumberString(n)=> Const(n.toDouble, c.Real)})
	def val_string = accept("string", {case CharString(n) => Const(n, c.String)})
	def val_true = (True ^^^ Const(true, c.Boolean))
	def val_false= (False ^^^Const(false,c.Boolean))

	// For comprehensions

	def seq_exp = OpenBracket~>(for_comprehension|seq_elements)<~CloseBracket
	def for_comprehension = exp~rep1(comp_element)
	def comp_element = comp_for|comp_if
	def comp_for = For~>opt(Each)~>OpenParen~>formalparam~In~exp
	def comp_if = If~bracketedsingleexp
	
	def seq_elements = repsep(exp, Comma)


	// DEFINITIONS

	def definitions = opt(definition~rep(opt(Semicolon)~>definition)) ^^ {
		case None => (Nil, Nil, Nil)
		case Some(def1~rest) => segregateDefinitions(def1::rest)
	}
	def segregateDefinitions(ds: List[Node]) = {
		import scala.collection.mutable.ListBuffer
		val vars = new ListBuffer[ValueDef]
		val types = new ListBuffer[TypeLikeDef]
		val traitInstances = new ListBuffer[TraitInstance]
		for (x <- ds) x match {
			case d: ValueDef => vars += d
			case t: TypeLikeDef => types += t
			case i: TraitInstance => traitInstances += i
		}
		(vars.toList, types.toList, traitInstances.toList)
	}

	def definition: Parser[Decl] = defn_var | defn_function | typelike_def | trait_inst

	// Variables

	def defn_var = Var~>defname~opt(typeannot)~Equals~exp ^^ {
		case name~typ~Equals~value => VariableDef(name, typ, value)
	}


	// NAMED FUNCTIONS AND LAMBDAS
/*	def defn_function = prefix_function
	
	def prefix_function = Function~>defname~!formalparamlist~!opt(typeannot)~function_body ^^ {
		case name~paramlist~res~body => {
			val argtype = paramtype(paramlist map (_.parsedType));
			FunctionDefn(name, false, FunctionTypeExp(argtype, res), Lambda(body, paramlist: _*))
		}
	}
*/
	def defn_function = function_signature ~ function_body ^^ {
		case (name, infix, isImplicit, typ, params)~body => FunctionDef(name, infix, isImplicit, typ, Lambda(body, params, Nil))
	}
	
	def function_signature =
		prefix_function_signature | infix_function_signature
	
	// Need this type declaration here, otherwise the Scala type inferencer gets very confused.
	def prefix_function_signature: Parser[Tuple5[String, Boolean, Boolean, FunctionTypeExp, Seq[Param]]] =
		opt(Implicit)~Function~!defname~!formalparamlist~!opt(typeannot) ^^ {
			
		case impl~Function~name~paramlist~res => {
			val argtype = paramtype(paramlist map (_.declaredType));
			(name, false, impl.isDefined, FunctionTypeExp(argtype, res), paramlist)
		}
	}

	// Need this type declaration here, otherwise the Scala type inferencer gets very confused.
	def infix_function_signature: Parser[Tuple5[String, Boolean, Boolean, FunctionTypeExp, Seq[Param]]] =
		Function~>formalparamsingle~!defname~!formalparamsingle~!opt(typeannot) ^^ {
			
		case param1~name~param2~res => {
			val argtype = TupleTypeExp(Array(param1.declaredType, param2.declaredType))
			val params = List(param1, param2);
			(name, true, false, FunctionTypeExp(argtype, res), params)
		}
	}
		
	def anon_fn = Function~>formalparamlist~opt(typeannot)~function_body ^^ {
		case paramlist~res~body => {
			Lambda(body, paramlist, Nil)
		}
	}
	
	def paramtype(params: Seq[TypeExp]): TypeExp =
		params.lengthCompare(1) match {
			case x if x<0 => UnitTypeExp
			case x if x==0=> params(0)
			case x if x>0 => TupleTypeExp(params)
		}

	def defname = accept("name definition", {
			case Ident(n) => n
			case Equals => "=" // Allow '=' as a valid function name.
		})
	def formalparamsingle = OpenParen~>formalparam<~CloseParen
	def formalparamlist = OpenParen~>repsep(formalparam, Comma)<~CloseParen
	def formalparam = defname~opt(typeannot) ^^
		{case ident~optionalType => Param(ident, optionalType)}
	def function_body = Equals~>exp | expblock

	
	// TYPES

	/** A type annotation on an expression/variable/parameter/function. */
	def typeannot = Colon~>typeexp

	def typelike_def: Parser[TypeLikeDef] = (
		typedef
	|	trait_decl
	)

	def typedef = Type~>defname~Equals~typeexp ^^ {
		case name~Equals~exp => TypeDef(name, exp)
	}

	def trait_decl = Trait~>defname~type_param~opt(Extends~>rep1sep(traitref, Comma))~trait_decl_block ^^ {
		case traitname~param~optSuperTraits~decls => {
			val supertraits = optSuperTraits.getOrElse(Nil)
			TraitDef(traitname, param, supertraits, decls)
		}
	}
	def trait_inst: Parser[TraitInstance] = Treat~>typeexp~As~rep1sep(traitref, Comma)~trait_inst_block ^^ {
		case t~As~traits~decls => TraitInstance(t, traits, decls)
	}

	def trait_decl_block = OpenBrace~>rep(defn_function | abstract_function)<~CloseBrace
	def trait_inst_block = OpenBrace~>rep(defn_function)<~CloseBrace

	def abstract_function = function_signature ^^ {
		case (name, infix, isImplicit, typ, params) => AbstractFunctionDef(name, infix, isImplicit, typ)
	}

	// GENERIC TYPE DECLARATIONS

	def type_param_list = rep1sep(type_param, Comma)

	def type_param = typename~rep(typeconstraint) ^^ {
		case name~constraints => TypeParam(name, constraints)
	}

	def typeconstraint: Parser[TypeConstraint] = (
		implementsconstraint
	|	subtypeconstraint
	|	supertypeconstraint
	)

	def implementsconstraint: Parser[TypeConstraint] = Colon~>traitref ^^ {case t => ConformsToTrait(t)}
	def subtypeconstraint: Parser[TypeConstraint] = SubtypeOf~>typeexp ^^ {case t => IsSubtypeOf(t)}
	def supertypeconstraint: Parser[TypeConstraint] = SupertypeOf~>typeexp ^^ {case t => IsSupertypeOf(t)}

	
	// TYPE EXPRESSIONS

	def typeexp = typeterm~rep(GoesTo~>typeterm) ^^
		{case (a~list) => composeFunctionType(a, list)}

	def typeterm: Parser[TypeExp] = tupletype | typeref

	def simpletyperef = typename ^^ {case name => TypeLikeRef(name, Nil)}

	def traitref = typename ^^ {case name => TraitRef(name)}

	def typeref = typename~opt(OpenBracket~>rep1sep(typeexp, Comma)<~CloseBracket) ^^ {
		case name~optParams => TypeLikeRef(name, optParams.getOrElse(Nil))
	}
	
	def typename = accept("type name", {case Ident(n) => n})
	
	def tupletype = OpenParen~>repsep(typeexp, Comma)<~CloseParen ^^
		(list => list.lengthCompare(1) match {
			case x if x<0 => UnitTypeExp
			case 0 => list(0)
			case x if x>0 => TupleTypeExp(list)
		})

	def composeFunctionType(head: TypeExp, tail: List[TypeExp]): TypeExp = tail match {
		case Nil => head
		case x::xs => composeFunctionType(FunctionTypeExp(head, x), xs)
	}

}
