package test.collectors

import org.scalatest.FlatSpec
import scala.io.Source
import curam.utils.logana.TraceCollector

class CollectorsSuite extends FlatSpec {
  val sourcePath = "/Java-JEE/Oracle/Middleware12/user_projects/domains/SIRECDomain/servers/AdminServer/logs/CuramAppLog.log"

  def source = Source.fromFile(sourcePath)

  "allTraces" should "produce some traces" in {
    assert(new TraceCollector(source).allTraces().size > 0)
  }
  "collectSQL" should "produce some traces" in {
    val extracted = new TraceCollector(source).collectSQLAsString()
    assert(extracted.size > 0 && extracted.forall { x ⇒ !x.contains("Trace") })
  }

}
