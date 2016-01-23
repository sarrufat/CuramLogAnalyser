package org.sarrufat.curam.log.analizer

import scala.io.Source

trait Statement {
  val statemet: String
  val date: String
  val seq: Int
}
trait SqlStatement extends Statement {
  val table: String
}

case class SqlSelect(val statemet: String, val date: String, val seq: Int) extends SqlStatement {
  var rows: Option[SqlNRows] = None
  lazy val table: String = {
    val regex = """SELECT.*FROM.*(\w).*""".r
    statemet match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlInsert(val statemet: String, val date: String, val seq: Int) extends SqlStatement {
  lazy val table: String = {
    val regex = """INSERT INTO.*(\w).*""".r
    statemet match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlUpdate(val statemet: String, val date: String, val seq: Int) extends SqlStatement {
  lazy val table: String = {
    val regex = """UPDATE.*(\w).*""".r
    statemet match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlDelete(val statemet: String, val date: String, val seq: Int) extends SqlStatement {
  lazy val table: String = {
    val regex = """DELETE FROM.*(\w).*""".r
    statemet match {
      case regex(t) ⇒ t
      case _        ⇒ ""
    }
  }
}
case class SqlNRows(val statemet: String, val date: String, val seq: Int, val rows: Int) extends SqlStatement {
  val table = ""
}
case class SqlOther(val statemet: String, val date: String, val seq: Int) extends SqlStatement {
  val table = ""
}

class TraceCollector(val source: Source) {
  def allTraces(): List[(String, Int)] = {
    (for {
      line ← source.getLines()
      if line.contains("Trace")
    } yield line).toList.zipWithIndex
  }

  def allTraces(condition: String ⇒ Boolean): List[(String, Int)] = {
    for {
      tr ← allTraces()
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
        case extractRegx(d, s) ⇒ new Statement { val statemet = s; val date = d; val seq = tup._2 }
        case _                 ⇒ new Statement { val statemet = tup._1; val date = ""; val seq = tup._2 }
      }
    }
    val res = for (st ← statemens) yield {
      val spl = st.statemet.split(" ")
      spl(0) match {
        case "SELECT" ⇒ SqlSelect(st.statemet, st.date, st.seq)
        case "INSERT" ⇒ SqlInsert(st.statemet, st.date, st.seq)
        case "UPDATE" ⇒ SqlUpdate(st.statemet, st.date, st.seq)
        case "DELETE" ⇒ SqlDelete(st.statemet, st.date, st.seq)
        case "#rows"  ⇒ SqlNRows(st.statemet, st.date, st.seq, spl(1).toInt)
        case _        ⇒ SqlOther(st.statemet, st.date, st.seq)
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
