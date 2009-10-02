/*
 * MovementGranularity.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui

/**
 * Indicates size of movement, e.g., from pressing arrow keys or page up/down.
 */
sealed abstract class MovementGranularity
case object ByOne extends MovementGranularity
case object ByBlock extends MovementGranularity
