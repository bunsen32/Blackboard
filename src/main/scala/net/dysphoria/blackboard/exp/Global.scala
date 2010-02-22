/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
/*
package net.dysphoria.blackboard.exp

import blackboard.data.types
import java.io.InputStreamReader
import scala.util.parsing.input.StreamReader

object Global {

	val res = this.getClass.getClassLoader.getResourceAsStream("Prelude.exp")
	val in = StreamReader(new InputStreamReader(res, "utf-8"))
	val Scope = GrammarParser.parseDefinitions(in) match {
		case GrammarParser.Success((v, t, b), _) => {
			val scope = new ParsedSyntax.Scope(ParsedSyntax.Const((), types.Unit), v, t, b)
			val resolved = NameResolver.resolve(scope)(new RootEnv)
			resolved
		}
		case failure => error("Cannot parse Prelude: " + failure)
	}
}
*/