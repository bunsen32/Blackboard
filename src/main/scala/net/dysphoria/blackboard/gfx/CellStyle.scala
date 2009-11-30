/*
 * CellStyle.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.dysphoria.blackboard.gfx

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._

/**
 * Represents the drawing style of a single table/dimension cell. Suspect that
 * this class may not live long. Need a more flexible/efficient mechanism for
 * specifying cell styles.
 */
class CellStyle {
	var fontFamily: String = "Helvetica"
	var fontSize: Float = 12F*256F
	var fontStyle: Int = SWT.NORMAL // SWT style flags
	var backgroundColor: RGB = new RGB(255, 255, 255)
	var color: RGB = new RGB(0, 0, 0) // American spelling: consistancy with CSS
	var textAlign: TextAlign = TextAlignCenter
	var marginLeft: Int = 0
	var marginRight: Int = 0
}

sealed abstract class TextAlign
case object TextAlignLeft extends TextAlign
case object TextAlignRight extends TextAlign
case object TextAlignCenter extends TextAlign
