/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

sealed abstract class Fixity {def operator: Boolean}
object Infix extends Fixity {def operator = true}
object Postfix extends Fixity {def operator = true}
/**
 * Represents a value which is not (or may not be) a function, or if a function,
 * is a 'prefix' one (i.e., applied as per conventional "f(args)")
 */
object Nofix extends Fixity {def operator = false}