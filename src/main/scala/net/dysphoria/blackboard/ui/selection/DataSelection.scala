/**
 * Created by IntelliJ IDEA.
 * User: andrew
 * Date: 10-Jan-2010
 * Time: 17:31:20
 * To change this template use File | Settings | File Templates.
 */

package net.dysphoria.blackboard.ui.selection

import net.dysphoria.blackboard._
import ui._
import ui.model._

abstract class DataSelection extends Selectable {
	/**
	 * Which Displayable are we talking about?
	 */
	def displayable: DisplayableInstance

	/**
	 * If this is a multiple selection, might have a set or range of values for particular
	 * axes. This method returns the coordinates which are unambiguous,
	 * i.e., which have only a single value per axis.
	 */
	def unambiguousCoords: Map[Axis, Int]
}
