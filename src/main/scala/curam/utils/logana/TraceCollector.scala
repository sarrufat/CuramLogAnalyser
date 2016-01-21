package curam.utils.logana

import scala.io.Source

class TraceCollector(val source: Source) {
  def allTraces() = {
    (for {
      line ← source.getLines()
      if line.contains("Trace")
    } yield line).toList
  }

  def allTraces(condition: String ⇒ Boolean) = {
    (for {
      line ← source.getLines()
      if line.contains("Trace") && condition(line)
    } yield line.stripLineEnd).toList
  }
  private def stripWithDates(tline: String) = {
    val extractRegx = """\[.*\] \[.*\] \[(.*)\] \[.*\](.*)""".r
    tline match {
      case extractRegx(d, rest) ⇒ d + rest
      case _                    ⇒ tline
    }
  }
  def collectSQLAsString() = allTraces(_.contains("SQL:")).map(stripWithDates(_))
}
