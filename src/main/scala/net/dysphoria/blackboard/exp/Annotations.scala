/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

import net.dysphoria.blackboard._
import data.{types=>t}

object Annotations {
	case class OperatorInfo(fixity: Fixity, precedence: Int){
		def isOperator = fixity != Nofix
		def infix = fixity == Infix
		def postfix = fixity == Postfix
	}
	object NoOperatorInfo extends OperatorInfo(Nofix, Precedence.ofFunctionApplication)

	object ResolvedRef extends Property[Ast.Identity]
	object OperatorInfo extends Property[OperatorInfo]
	object RefIsTrait extends Property[Boolean]

	object Type extends Property[t.Type]
	object Trait extends Property[t.Trait]
}
