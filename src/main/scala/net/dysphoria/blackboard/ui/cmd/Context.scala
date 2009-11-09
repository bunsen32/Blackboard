/*
 * Context.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.cmd

import org.eclipse.swt.widgets.Shell

/**
 * A context for a command. Includes a notion of current window/ViewCanvas.
 */
class Context(val window: Shell, val canvas: ViewCanvas)
