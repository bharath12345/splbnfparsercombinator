package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.Funcs._
import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.{ColColumnParameter, ColumnParameter, DoubleColumnParameter, LongColumnParameter, StringColumnParameter}
import com.glassbeam.scalar.core.parser.{DataColumn, Funcs}
import com.glassbeam.scalar.core.spl.lexer.{COLUMN, SplTableToken}
import com.glassbeam.scalar.model.ColumnType.ColumnType
import com.glassbeam.scalar.model._
import com.glassbeam.scalar.utils.StripUtils._
import com.glassbeam.scalar.utils.StringUtils._

import scala.collection.immutable.{List, Vector}
import scala.util.control.NonFatal
import scala.util.matching.Regex

object ColOp extends Logger {
  final lazy val logger = Logging(this)

  trait ColumnParameter {
    type T
    val value: T
    require(value != null)

    val regex: Regex = null
    val column: COLUMN = null
    val func: Funcs = null
    val name: String = value.toString

    override def toString = value.toString
    def getValue: DataValue = EmptyValue
  }

  case class StringColumnParameter(override val value: StringValue) extends ColumnParameter {
    type T = StringValue
    override def getValue: DataValue = value
  }

  case class LongColumnParameter(override val value: LongValue) extends ColumnParameter {
    type T = LongValue
    override def getValue: DataValue = value
  }

  case class DoubleColumnParameter(override val value: DoubleValue) extends ColumnParameter {
    type T = DoubleValue
    override def getValue: DataValue = value
  }

  case class RegexColumnParameter(override val value: Regex) extends ColumnParameter {
    type T = Regex
    override val regex = value
  }

  case class ColColumnParameter(override val value: COLUMN) extends ColumnParameter {
    type T = COLUMN
    override val name = value.column_name
    override val column = value
    override def getValue = throw new Exception(s"getValue called on raw column: $value")
  }

  case class ColFuncColumnParameter(override val value: Funcs) extends ColumnParameter {
    type T = Funcs
    override val func: Funcs = value
  }

}

abstract class ColOpFunction(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int) {

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit]

  def getColumnForCOLUMN(incoming: COLUMN, COS: ColOpSharables): DataColumn = {
    COS.dataColumns(incoming.column_name)
  }

  // ToDo: This whole function is UGLY and needs to be REMOVED
  protected def ColString(x: ColumnParameter): Option[String] = {
    x match {
      case y: StringColumnParameter => Option(y.getValue.toString)
      case l: LongColumnParameter => Option(l.getValue.toString)
      case d: DoubleColumnParameter => Option(d.getValue.toString)
      case x => throw new Exception(s"value request from non literal column = $x")
    }
  }

  protected def NumericLong(x: String): Long = {
    if (x != null && x.nonEmpty) x.toLong
    else 0L
  }

  protected def NumericDouble(x: String): Double = {
    if (x != null && x.nonEmpty) x.toDouble
    else 0
  }

  protected def numericPerColtype(ddl: String, numeric: Double): DataValue = {
    try {
      ddl match {
        case "i32" => LongValue(numeric.toInt)
        case "i64" => LongValue(numeric.toLong)
        case "r32" => DoubleValue(numeric.toFloat)
        case "r64" => DoubleValue(numeric.toDouble)
        case _ => StringValue(numeric.toString)
      }
    } catch {
      case NonFatal(ex) =>
        logger.warning(SIM.mpspath, s"From numericPerColtype: ddl=$ddl, value=$numeric, error=${ex.getLocalizedMessage}")
        EmptyValue
    }
  }

  protected def exceptionMsg(e: Option[Throwable]): String = e match {
    case Some(e) => s"Exception: ${e.getLocalizedMessage}"
    case None => ""
  }

  protected def warningString(func: String, e: Option[Throwable]) = s"Column Operation Warning for $func: " +
    s"splline = ${splline}, op = $op, param = $param, table = ${COS.table_name}, column params = $colparam. ${exceptionMsg(e)}"
}

trait ColOpTrait {

  import ColOp._

  val op: ColumnOps
  val param: String
  val splline: Int

  private var execute: (SharedImmutables, ColOpSharables) => Unit = null
  private var colparam = Vector.empty[ColumnParameter]

  def verify(columns: List[COLUMN]): Unit = {
    def colParam(p: String) {
      if (p == null || p.isEmpty) {
        throw new Exception(s"COL parameter empty, l# $splline")
        colparam = colparam :+ StringColumnParameter(StringValue(""))
      } else if (p.head == '\'') {
        // 'literal'
        colparam = colparam :+ StringColumnParameter(StringValue(stripQuotes(p)))
      } else if (p.isLongNumber) {
        // number-literal
        colparam = colparam :+ LongColumnParameter(LongValue(p.toLong))
      } else if(p.isDoubleNumber) {
        colparam = colparam :+ DoubleColumnParameter(DoubleValue(p.toDouble))
      } else if (p.head == '/') {
        // /pattern/ /pattern/g
        if (p.last == '/')
          colparam = colparam :+ RegexColumnParameter(new Regex(stripQuotes(p)))
        else
          throw new Exception(s"SPL regexes do not take Perl modifiers $p, l# $splline")
      } else if (p.head.isUpper) {
        // FUNC
        try {
          colparam = colparam :+ ColFuncColumnParameter(Funcs.withName(p))
        } catch {
          case e: java.util.NoSuchElementException =>
            throw new Exception(s"Unknown COLCALC function: $p, l# $splline")
        }
      } else if (p.head.isLower) {
        // column
        COS.rawColumns.get(p) match {
          case Some(c) =>
            colparam = colparam :+ ColColumnParameter(c)
          case None =>
            throw new Exception(s"COL parameter: column $p not found, l# $splline")
            dumpCols
        }
      } else {
        throw new Exception(s"COL parameter ($p) does not match Literal/Numeric/Pattern/Func/Column, l# $splline")
      }
    }

    logger.debug(SIM.mpspath, s"ColOp: op = $op, param=$param, splline=$splline, table = ${COS.table_name}")
    if (op != CONSTRAIN && op != COLCASE && op != COLEND && op != COLELSE) {
      val qp = Qsplitter(param)
      logger.debug(SIM.mpspath, s"Qsplitter result=$qp")
      for (p <- qp) {
        colParam(p.trim)
      }
    }
    logger.debug(SIM.mpspath, s"colparam=$colparam")

    val opInstance = op match {
      //case ROWSPLIT => new ColRowSplit(colparam, op, param, splline)
      //case ADD_ROW_NUMBER => new ColAddRowNumber(colparam, op, param, splline)
      //case ROWDROP => new RowDrop(colparam, op, param, splline)
      case COLFILL => new ColFill(colparam, op, param, splline)
      case COLDROP => new ColDrop(colparam, op, param, splline)
      case COLJOIN => new ColJoin(colparam, op, param, splline)
      case COLREP => new ColRep(colparam, op, param, splline)
      case COLSPLIT => new ColSplit(colparam, op, param, splline)
      case COLHIERARCHY => new ColHierarchy(colparam, op, param, splline)
      case COLCOPY => new ColCopy(colparam, op, param, splline)
      case COLMAP => new ColMap(colparam, op, param, splline)
      case COLCALC => new ColCalc(colparam, op, param, splline)
      case COLBOUND => new ColBound(colparam, op, param, splline)
      case CONSTRAIN => new Constrain(colparam, op, param, splline)
      case COLASSERT => new ColAssert(colparam, op, param, splline)
      case COLPRINT => new ColPrint(colparam, op, param, splline)
      case COLCASE => new ColCase(colparam, op, param, splline)
      case COLWHEN => new ColWhen(colparam, op, param, splline)
      case COLELSE => new ColElse(colparam, op, param, splline)
      case COLEND => new ColEnd(colparam, op, param, splline)
      case COLLOOKUPBYNAME => new ColLookupByName(colparam, op, param, splline)
      case COLLOOKUPBYPOSITION => new ColLookupByPosition(colparam, op, param, splline)
      case COLCOUNT => new ColCount(colparam, op, param, splline)
      case COLBRANCH => new ColBranch(colparam, op, param, splline)
      case COLFILE => new ColFile(colparam, op, param, splline)
      case COLPUSH => new ColPush(colparam, op, param, splline)
      case x => throw new Exception(s"Unsupported column operation $x")
    }

    val vvv: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = opInstance.verify
    if (!vvv.isDefinedAt(op)) throw new Exception(s"ColOp ($op) not understood, l# $splline")

    execute = vvv(op)
  }

  private def dumpCols {
    if (COS.dataColumns.size < 1)
      logger.error(SIM.mpspath, "No Columns", true)
    else {
      var Cols: String = COS.dataColumns.head._1
      if (COS.dataColumns.size > 1)
        for (c <- COS.dataColumns.tail)
          Cols += ", " + c._1
      logger.debug(SIM.mpspath, s"Columns: $Cols")
    }
  }

  // Splitting parameters... quite specialized -- this does not need to be fast, because it's done only at compile time
  // Split at commas... ONLY when commas are found outside "quoting"
  // Quoting /xx/  'xx'  "xx"
  private def Qsplitter(x: String): List[String] = {
    var str = "" // string to build A parameter
    val nonquote = '~' // a character that is not used (anywhere)
    var typ = nonquote // non-quote
    var pchr = nonquote // non-quote
    var res = Vector.empty[String]
    for (c <- x) {
      if (typ != nonquote) {
        // we're inside quotes
        str += c
        if (c == typ && pchr != '\\') typ = nonquote // end quote
      } else if (c == ',') {
        // Found a standalone comma -- split
        if (str.nonEmpty) {
          // Have we found a parameter?  don't care for empties...
          res = res :+ str // Add to the list
          str = "" // reset the collector
        }
      } else {
        str += c
        if (c == '/' || c == '\'' || c == '\"') typ = c
      }
      pchr = c
    }
    // Done with the whole string; anything left over?
    if (str.nonEmpty) res = res :+ str
    res.toList
  }

  def flush(): Unit = {
    op match {
      case COLCOUNT => //sess_count = 0 For each Session
        val column_name = colparam(2).column.column_name
        val column: DataColumn = COS.dataColumns(column_name)
        column.sess_count = 0
        column.setValue(LongValue(column.sess_count))
      case _ =>
    }
  }

  def exec(): Unit = {
    try {
      logger.debug(SIM.mpspath, s"COLOP/exec ($op) got called for table = ${COS.table_name}, cases = ${COS.cases}")
      if ((COS.cases == NOCASE || COS.cases == CASETHEN) || (op == COLCASE || op == COLWHEN ||
        op == COLELSE || op == COLEND)) {
        execute(null, null)
      }
    } catch {
      case NonFatal(e) =>
        logger.error(e, SIM.mpspath, s"COLOP/exec ($op) failed for table = ${COS.table_name})", true)
    }
  }
}
