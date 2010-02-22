/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard

class InternalFault(msg: String) extends RuntimeException(msg)

class NotImplementedException extends InternalFault("Not implemented")