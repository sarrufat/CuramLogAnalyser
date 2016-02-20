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

  val tables: Seq[String] = Seq()
  @BeanProperty
  lazy val stables = tables.mkString(",")
  @BeanProperty
  lazy val numberrows = 0
  @BeanProperty
  val wherecond = ""
}

case class SqlSelect(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  var rows: Option[SqlNRows] = None
  override val tables: Seq[String] = {
    val regex = """SELECT.*FROM (\w+).*""".r
    val leftOuter = """SELECT.*FROM (\w+).*LEFT OUTER JOIN (\w+).*""".r
    val two = """SELECT.*FROM (\w+) , (\w+).*""".r
    val three = """SELECT.*FROM (\w+) , (\w+) , (\w+).*""".r
    val four = """SELECT.*FROM (\w+) , (\w+) , (\w+) , (\w+).*""".r
    val five = """SELECT.*FROM (\w+) , (\w+) , (\w+) , (\w+) , (\w+).*""".r
    val six = """SELECT.*FROM (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+).*""".r
    val seven = """SELECT.*FROM (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+).*""".r
    val eight = """SELECT.*FROM (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+).*""".r
    val nine = """SELECT.*FROM (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+) , (\w+).*""".r

    statement match {
      case nine(s1, s2, s3, s4, s5, s6, s7, s8, s9) ⇒ Seq(s1, s2, s3, s4, s5, s6, s7, s8, s9)
      case eight(s1, s2, s3, s4, s5, s6, s7, s8)    ⇒ Seq(s1, s2, s3, s4, s5, s6, s7, s8)
      case seven(s1, s2, s3, s4, s5, s6, s7)        ⇒ Seq(s1, s2, s3, s4, s5, s6, s7)
      case six(s1, s2, s3, s4, s5, s6)              ⇒ Seq(s1, s2, s3, s4, s5, s6)
      case five(s1, s2, s3, s4, s5)                 ⇒ Seq(s1, s2, s3, s4, s5)
      case four(s1, s2, s3, s4)                     ⇒ Seq(s1, s2, s3, s4)
      case three(s1, s2, s3)                        ⇒ Seq(s1, s2, s3)
      case two(s1, s2)                              ⇒ Seq(s1, s2)
      case leftOuter(t, l)                          ⇒ Seq(t, l)
      case regex(t)                                 ⇒ Seq(t)
      case _                                        ⇒ Seq()
    }
  }
  override lazy val numberrows = rows match { case Some(r) ⇒ r.rows; case None ⇒ -1 }
  override val wherecond: String = {
    val regex = """SELECT.*FROM .* WHERE (.+)""".r
    val regex2 = """SELECT.*FROM .* WHERE\((.+)\)""".r
    statement match {
      case regex(t)  ⇒ t
      case regex2(w) ⇒ w
      case _         ⇒ ""
    }
  }
}
case class SqlInsert(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  override val tables: Seq[String] = {
    val regex = """INSERT INTO (\w+).*""".r
    statement match {
      case regex(t) ⇒ Seq(t)
      case _        ⇒ Seq()
    }
  }
}

case class SqlUpdate(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  override val tables: Seq[String] = {
    val regex = """UPDATE (\w+).*""".r
    statement match {
      case regex(t) ⇒ Seq(t)
      case _        ⇒ Seq()
    }
  }
}
case class SqlDelete(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {
  override val tables: Seq[String] = {
    val regex = """DELETE FROM (\w+).*""".r
    statement match {
      case regex(t) ⇒ Seq(t)
      case _        ⇒ Seq()
    }
  }
}
case class SqlNRows(override val statement: String, override val date: String, override val seq: Int, val rows: Int) extends SqlStatement {
}
case class SqlOther(override val statement: String, override val date: String, override val seq: Int) extends SqlStatement {

}

object TraceCollector {
  def getTableList(statemens: List[SqlStatement]) = statemens.flatMap(_.tables).groupBy { x ⇒ x }.map(_._1).toList.sorted
  val SQLINSTRUCTIONS = Seq("INSERT", "UPDATE", "SELECT", "DELETE")
  def getByType(statemens: List[SqlStatement], otyp: Option[String]): List[SqlStatement] = {
    otyp match {
      case Some(typ) ⇒ SQLINSTRUCTIONS.indexOf(typ) match {
        case 0 ⇒ statemens.filter { x ⇒ x match { case s: SqlInsert ⇒ true; case _ ⇒ false } }
        case 1 ⇒ statemens.filter { x ⇒ x match { case s: SqlUpdate ⇒ true; case _ ⇒ false } }
        case 2 ⇒ statemens.filter { x ⇒ x match { case s: SqlSelect ⇒ true; case _ ⇒ false } }
        case 3 ⇒ statemens.filter { x ⇒ x match { case s: SqlDelete ⇒ true; case _ ⇒ false } }
        case _ ⇒ List()
      }
      case None ⇒ statemens
    }
  }
}

class TraceCollector(val source: Source) {
  lazy val allTraces: List[(String, Int)] = {
    val all = (for {
      line ← source.getLines()

    } yield line).toList.zipWithIndex.map(t ⇒ (t._1, t._2 + 1))
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
    res.filterNot { x ⇒ x match { case n: SqlNRows ⇒ true; case _ ⇒ false } }
  }
}
