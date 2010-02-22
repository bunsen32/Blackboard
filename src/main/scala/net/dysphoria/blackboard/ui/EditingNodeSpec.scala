/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui

/**
 * Immutable spec of an ‘editing node’, several of which may float around a selection.
 */
case class EditingNodeSpec(position: CompassPosition, glyph: NodeGlyph, f: Function0[Unit])
