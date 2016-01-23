package org.sarrufat.curam.log.analizer

import scala.io.Source
import scala.beans.BeanProperty
import scala.util.Try

trait Statement {
  @BeanProperty
  val statement: String = ""
  @BeanProperty
  val date: String = ""
  @BeanProperty
  val seq: Int = 0
  protected val itype = Try("""\w*""".r.findAllIn(statement).toSeq.head).getOrElse("")
  @BeanProperty
  val stype: String = itype
}
trait SqlStatement extends Statement {
  @BeanProperty
  val table: String = ""
}

case class SqlSelect(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  var rows: Option[SqlNRows] = None
  override val table: String = {
    val regex = """SELECT.*FROM (\w+).*""".r
    statement match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlInsert(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  override val table: String = {
    val regex = """INSERT INTO (\w+).*""".r
    statement match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}

case class SqlUpdate(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  override val table: String = {
    val regex = """UPDATE (\w+).*""".r
    statement match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlDelete(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  override val table: String = {
    val regex = """DELETE FROM (\w+).*""".r
    statement match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlNRows(override val statement: String, override val date: String, override val seq: Int, val rows: Int) extends SqlStatement {
}
case class SqlOther(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {

}

class TraceCollector(val source: Source) {
  lazy val allTraces: List[(String, Int)] = {
    val all = (for {
      line ← source.getLines()

    } yield line).toList.zipWithIndex
    all.filter(_._1.contains("Trace"))
  }

  def allTraces(condition: String ⇒ Boolean): List[(String, Int)] = {
    for {
      tr ← allTraces
      if (condition(tr._1))
    } yield tr
  }
  private def stripWithDates(tline: String) = {
    val extractRegx = """\[.*\] \[.*\] \[(.*)\] \[.*\](.*)""".r
    tline match {
      case extractRegx(d, rest) ⇒ d + rest
      case _                    ⇒ tline
    }
  }
  def collectSQLAsString() = allTraces(_.contains("SQL:")).map(tr ⇒ (stripWithDates(tr._1), tr._2))

  def collectSQL(): List[SqlStatement] = {
    val extractRegx = """(.*) \- SQL: (.*)""".r
    val statemens = collectSQLAsString().map { tup ⇒
      tup._1 match {
        case extractRegx(d, s) ⇒ new Statement { override val statement = s; override val date = d; override val seq = tup._2 }
        case _                 ⇒ new Statement { override val statement = tup._1; override val date = ""; override val seq = tup._2 }
      }
    }
    val res = for (st ← statemens) yield {
      val spl = st.statement.split(" ")
      spl(0) match {
        case "SELECT" ⇒ SqlSelect(st.statement, st.date, st.seq)
        case "INSERT" ⇒ SqlInsert(st.statement, st.date, st.seq)
        case "UPDATE" ⇒ SqlUpdate(st.statement, st.date, st.seq)
        case "DELETE" ⇒ SqlDelete(st.statement, st.date, st.seq)
        case "#rows"  ⇒ SqlNRows(st.statement, st.date, st.seq, spl(1).toInt)
        case _        ⇒ SqlOther(st.statement, st.date, st.seq)
      }
    }
    // Process #rows for selects
    res.collect {
      case sel: SqlSelect ⇒ res.find(_.seq == sel.seq + 1).collect {
        case r: SqlNRows ⇒ sel.rows = Some(r)
      }
    }
    res
  }
}
