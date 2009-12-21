/*
 * TableActions.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.ui.actions

import org.eclipse.swt.SWT
import selection._

trait TableActions { self: ActionsHolder =>

	object GroupRowCols extends Action {
		def name = "Group "+rowColString
		override def accelerator = SWT.MOD1 | 'g'
		def isApplicable = currentSelection match {
			case labs: LabelRange => labs.axis.isInstanceOf[StructAxis]
			case _ => false
		}
		def safeApply = currentSelection match {
			case labs: LabelRange => 
				val merged = currentView.policy.groupStructLabels(labs)
				currentView.ui.select(merged)
				currentView.policy.updateDisplay
		}
	}

	object RepeatRowCol extends Action {
		def name = "Repeat "+rowColString
		def isApplicable = currentSelection match {
			case lab: OneLabel =>
				lab.axis.length == 1 ||
				lab.axis.isInstanceOf[StructAxis]
				
			case labs: LabelRange =>
				labs.axis.length == labs.range.length ||
				labs.axis.isInstanceOf[StructAxis]
				
			case _ => false
		}
		def safeApply {
			currentView.policy.labelInsertRepeatedAfter
		}
	}

	object InsertSimilarRowCol extends Action {
		def name = "Insert similar "+rowColString
		def isApplicable = currentSelection match {
			case lab: OneLabel => lab.axis.isInstanceOf[ArrayAxis]
			case _ => false
		}
		def safeApply {
			currentView.policy.labelInsertSimilarAfter
		}
	}

	object InsertDistinctRowCol extends Action {
		def name = "Insert distinct "+rowColString
		def isApplicable = currentSelection match {
			case lab: OneLabel =>
				lab.index == (lab.axis.length - 1) ||
				lab.axis.isInstanceOf[StructAxis]

			case labs: LabelRange =>
				labs.range.last == (labs.axis.length - 1) ||
				labs.axis.isInstanceOf[StructAxis]
				
			case _ => false
		}
		def safeApply {
			currentView.policy.labelInsertDifferentAfter
		}
	}
	
	object DeleteRowCols extends Action {
		def name = "Delete "+rowColString
		override def accelerator = SWT.SHIFT | SWT.DEL
		def isApplicable = currentSelection.isInstanceOf[LabelSelection]
		def safeApply {
			currentView.policy.labelDeleteData
		}
	}

	object DeleteAxis extends Action {
		def name = "Delete axis"
		override def accelerator = SWT.ALT | SWT.DEL
		def isApplicable = currentSelection.isInstanceOf[LabelSelection]
		def safeApply {
			currentView.policy.labelDeleteAxis
		}
	}

	object HideLabel extends Action {
		def name = "Hide label"
		def isApplicable = currentSelection match {
			case lab: OneLabel => (lab.block, lab.axis) match {
				case (block: StructBlock, axis: StructAxis) =>
					// If this axis is the last one in its block:
					lab.axisIndex == (block.axes(lab.orientation).length - 1) &&
					// And there are child axes:
					! block.elementFor(lab.coords).axes(lab.orientation).isEmpty

				case _ => false
			}

			case _ => false
		}
		def safeApply {
			currentSelection match {
				case lab: OneLabel => lab.axis match {
					case s: StructAxis =>
						s.elements(lab.index) = (s.label(lab.index), false)
						currentView.policy.updateDisplay
						
					case _=>
				}
				case _ =>
			}
		}
	}

	object ShowHiddenLabels extends Action {
		def name = "Show hidden label"

		def isApplicable = {
			currentSelection match {
				case lab: LabelSelection =>
					val o = lab.orientation
					val coords = lab.allCoordsButLast
					def recurse(b: TableBlock): Boolean = b match {
						case s: StructBlock =>
							val axis = s.structAxis
							coords.contains(axis) && {
								val i = coords(axis)
								// (Less efficient, but matches structure of safeApply.)
								recurse(s.elements(i)) ||
									!axis.visible(i)
							}
						case _ => false
					}
					recurse(currentView.table.topBlock)
				case _ => false
			}
		}
		def safeApply {
			currentSelection match {
				case lab: LabelSelection =>
					val o = lab.orientation
					val coords = lab.allCoordsButLast
					def recurse(b: TableBlock): Boolean = b match {
						case s: StructBlock =>
							val axis = s.structAxis
							coords.contains(axis) && {
								val i = coords(axis)
								recurse(s.elements(i)) || {
									val action = !axis.visible(i)
									if (action) {
										s.structAxis.elements(i) = (axis.label(i), true)
										currentView.policy.updateDisplay
									}
									action
								}
							}
						case _ => false
					}
					recurse(currentView.table.topBlock)
				case _ => //ignore
			}
		}
	}

	private def rowColString = currentSelection match {
		case lab: LabelRange if lab.orientation.isX => "columns"
		case lab: LabelRange if lab.orientation.isY => "rows"
		case lab: OneLabel if lab.orientation.isX => "column"
		case lab: OneLabel if lab.orientation.isY => "row"
		case _ => "row/col"
	}

}
