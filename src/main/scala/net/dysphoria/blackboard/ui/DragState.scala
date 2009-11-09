/*
 * DragState.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

abstract sealed class DragState
case object NoDrag extends DragState
/**
 * Represents the fact that the mouse is down, ready for dragging, but we have not
 * yet entered the drag state. <var>x</var> and <var>y</var> represent the mouse
 * coordinates relative to the current <var>UIState.focus<var>.
 */
case class MouseDown(x: Int, y: Int) extends DragState
/**
 * Whether we are in the midst of a 'drag' operation
 * (as opposed to merely mid-'click').
 */
case object Dragging extends DragState

