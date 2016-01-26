package test.collectors

import org.scalatest.FlatSpec
import scala.io.Source
import org.sarrufat.curam.log.analizer.TraceCollector
import org.sarrufat.curam.log.analizer.SqlNRows
import org.sarrufat.curam.log.analizer.SqlOther

class CollectorsSuite extends FlatSpec {
  val sourcePath = "/Java-JEE/Oracle/Middleware12/user_projects/domains/SIRECDomain/servers/AdminServer/logs/CuramAppLog.log"

  def source = Source.fromFile(sourcePath)

  "allTraces" should "produce some traces" in {
    assert(new TraceCollector(source).allTraces.size > 0)
  }
  "collectSQLAsString" should "produce some traces" in {
    val extracted = new TraceCollector(source).collectSQLAsString()
    assert(extracted.size > 0 && extracted.forall { x ⇒ !x._1.contains("Trace") })
  }

  "collectSQL" should "produce some traces" in {
    val extracted = new TraceCollector(source).collectSQL()
    assert(extracted.size > 0)
  }
  "collectSQL tables" should "produce tablenames" in {
    val extracted = new TraceCollector(source).collectSQL()
    assert(extracted.size > 0 && extracted.exists { _.tables.size > 0 })
    val someSql = extracted.filterNot { _ match { case r: SqlNRows ⇒ true; case o: SqlOther ⇒ true; case _ ⇒ false } }
    assert(someSql.forall { _.tables.size > 0 })
  }

}
