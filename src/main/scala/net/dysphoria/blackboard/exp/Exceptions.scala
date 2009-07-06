/*
 * Exceptions.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

class CompilationException(msg: String) extends Exception(msg)
class ParseError(msg: String) extends CompilationException(msg)
class TypeError(msg: String) extends CompilationException(msg)