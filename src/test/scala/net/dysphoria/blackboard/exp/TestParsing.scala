/*
 *  Part of Blackboard spreadsheet. ©2010 Andrew Forrest. See LICENSE file for details.
 */
package net.dysphoria.blackboard.exp

class TestParsing {
	import java.io._
	def withFileReaderSeries(clasz: Class[_], basename: String)(op: Reader=>Unit){
		withFileSeries(clasz, basename){file =>
			op(new InputStreamReader(file, "utf-8"))
		}
	}

	def withFileSeries(clasz: Class[_], basename: String)(op: InputStream=>Unit) {
		val loader = clasz.getClassLoader
		var i = 0
		while(true) {
			val stream = try {
				loader.getResourceAsStream(basename+i)
			}catch {
				case x: FileNotFoundException => return
			}
			try {
				op(stream)
			}finally{
				stream.close
			}
			i += 1
		}
	}

}
