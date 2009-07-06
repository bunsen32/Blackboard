/*
 * Exceptions.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard

class InternalFault(msg: String) extends RuntimeException(msg)