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
		def safeApply {
			// TODO
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
		override def accelerator = SWT.DEL
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
		def isApplicable = currentSelection.isInstanceOf[LabelSelection] //TODO
		def safeApply {
			// TODO
		}
	}

	object ShowHiddenLabels extends Action {
		def name = "Show hidden label"
		def isApplicable = currentSelection.isInstanceOf[LabelSelection] //TODO
		def safeApply {
			// TODO
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
