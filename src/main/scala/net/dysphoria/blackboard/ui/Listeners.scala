package net.dysphoria.blackboard.ui

import org.eclipse.swt
import swt.events._
import swt.widgets.{Event,Listener}

object Listeners {

	implicit def painter2listener(painter: (PaintEvent => Unit)) = new PaintListener {
		override def paintControl(e: PaintEvent) = painter(e)
	}

	implicit def disposer2listener(disposer: (DisposeEvent => Unit)) = new DisposeListener {
		override def widgetDisposed(e: DisposeEvent) = disposer(e)
	}

	implicit def function2listener(listener: (Event => Unit)) = new Listener {
		override def handleEvent(e: Event) = listener(e)
	}
}
