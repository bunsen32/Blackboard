/*
 * MenuManager.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.actions

import scala.collection.mutable
import net.dysphoria.blackboard.ui.Listeners._
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Event, Menu, MenuItem}

class MenuManager(val menu: Menu) {
	def this(item: MenuItem) = {
		this(new Menu(item))
		item.setMenu(menu)
	}

	val items = new mutable.HashMap[MenuItem, Action]

	menu.addListener(SWT.Show, (e: Event) ⇒ {
		for((item, action) ← items){
			item.setText(action.name)
			item.setEnabled(action.isApplicable)
		}
	})

	def add(action: Action) = {
		val item = new MenuItem(menu, SWT.PUSH)
		item.setAccelerator(action.accelerator)
		item.addListener(SWT.Selection, (e: Event) => action.apply())
		items += ((item, action))
	}

	def addSeparator = {
		new MenuItem(menu, SWT.SEPARATOR)
	}
}
