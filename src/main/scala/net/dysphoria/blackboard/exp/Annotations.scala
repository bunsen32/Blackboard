/*
 * Annotations.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.exp

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
