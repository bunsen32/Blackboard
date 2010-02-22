/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.ui.actions

import org.eclipse.swt.SWT
import net.dysphoria.blackboard._
import ui._
import ui.model._
import ui.selection._
import org.eclipse.swt.graphics.Point

trait TableActions { self: ActionsHolder =>

	object NewTable extends Action {
		def name = "New table"
		override def accelerator = SWT.MOD1 | 'n'
		def isApplicable = true
		def safeApply = {
			val group = currentView.model
			val newTable = new Table(new TableArrayData)
			group.add(new Point(0, group.size.y + 1024), newTable)
			currentView.modelUpdated
		}
	}

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
			case lab: LabelInstance =>
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
			case lab: LabelInstance => lab.axis.isInstanceOf[ArrayAxis]
			case _ => false
		}
		def safeApply {
			currentView.policy.labelInsertSimilarAfter
		}
	}

	object InsertDistinctRowCol extends Action {
		def name = "Insert distinct "+rowColString
		def isApplicable = currentSelection match {
			case lab: LabelInstance =>
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
			case lab: LabelInstance => (lab.ownerPartModel, lab.axis) match {
				case (block: TableStruct, axis: StructAxis) =>
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
				case lab: LabelInstance => lab.axis match {
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
					val coords = lab.unambiguousCoords
					def recurse(b: TablePart): Boolean = b match {
						case s: TableStruct =>
							val axis = s.structAxis
							coords.contains(axis) && {
								val i = coords(axis)
								// (Less efficient, but matches structure of safeApply.)
								recurse(s.elements(i)) ||
									!axis.visible(i)
							}
						case _ => false
					}
					recurse(lab.table.topBlock)
				case _ => false
			}
		}
		def safeApply {
			currentSelection match {
				case lab: LabelSelection =>
					val o = lab.orientation
					val coords = lab.unambiguousCoords
					def recurse(b: TablePart): Boolean = b match {
						case s: TableStruct =>
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
					recurse(lab.table.topBlock)
				case _ => //ignore
			}
		}
	}

	private def rowColString = currentSelection match {
		case lab: LabelRange if lab.orientation.isX => "columns"
		case lab: LabelRange if lab.orientation.isY => "rows"
		case lab: LabelInstance if lab.orientation.isX => "column"
		case lab: LabelInstance if lab.orientation.isY => "row"
		case _ => "row/col"
	}

}
