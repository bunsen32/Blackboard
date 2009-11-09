/*
 * pathToRegion.scala
 *
 * Part of
 */

package net.dysphoria.blackboard.gfx

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics._

/**
 * Copies a Path into a Region, using code modified from:
 * http://dev.eclipse.org/viewcvs/index.cgi/org.eclipse.swt.snippets/src/org/eclipse/swt/snippets/Snippet285.java?view=co
 */
object pathToRegion extends Function2[Region, Path, Unit] {

	def apply(region: Region, path: Path) {
		// Flatten the path into a new Path consisting only of straight-line segments.
		val flatPath = new Path(path.getDevice, path, 0.1F)
		val data = try {
			flatPath.getPathData
		}finally{
			flatPath.dispose
		}
		val points = data.points
		val types = data.types
		var start = 0
		var end = 0
		for (t <- types)
			t match {
				case SWT.PATH_MOVE_TO =>
					if (start != end) {
						val temp = new Array[Int](end - start);
						for (k <- start until end) {
							temp(k - start) = Math.round(points(k));
						}
						region.add(temp);
					}
					start = end;
					end += 2;

				case SWT.PATH_LINE_TO =>
					end += 2;

				case SWT.PATH_CLOSE =>
					if (start != end) {
						val temp = new Array[Int](end - start);
						for (k <- start until end) {
							temp(k - start) = Math.round(points(k));
						}
						region.add(temp);
					}
					start = end;
			}
	}
	
}
