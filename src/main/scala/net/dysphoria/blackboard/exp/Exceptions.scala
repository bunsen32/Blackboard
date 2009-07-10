/*
 * Exceptions.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

class CompilationException(msg: String) extends Exception(msg)
class ParseError(msg: String) extends CompilationException(msg)

import blackboard.data.types.Type

class TypeErrorException(msg: String) extends CompilationException(msg)
class TypeMismatchException(val expected: Type, val received: Type) extends
	TypeErrorException("Type mismatch: "+expected.toString+" ≠ "+received.toString)
class RecursiveTypeException(val typ: Type, val container: Type) extends
	TypeErrorException("Recursive unification: "+typ.toString+" cannot equal "+container.toString)