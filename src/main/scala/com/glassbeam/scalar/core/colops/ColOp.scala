package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.Funcs._
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser._
import ColOp.{ColColumnParameter, ColumnParameter, DoubleColumnParameter, LongColumnParameter, StringColumnParameter}
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

    val typ: ColumnType = null
    val regex: Regex = null
    val column: Column = null
    val func: Funcs = null
    val name: String = value.toString

    override def toString = value.toString

    def getValue: DataValue = EmptyValue
    def setValue(dv: DataValue): Unit = Unit

    def persist(p: Boolean): Unit = Unit

    def getPrev: DataValue = EmptyValue
    def setPrev(dv: DataValue): Unit = Unit
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

  case class ColColumnParameter(override val value: Column) extends ColumnParameter {
    type T = Column
    override val typ = value.typ
    override val name = value.column_name
    override val column = value

    override def getValue = value.getValue
    override def setValue(dv: DataValue) = value.setValue(dv)

    override def persist(p: Boolean) = value.persist = p

    override def getPrev = value.getPreviousValue
    override def setPrev(dv: DataValue): Unit = value.setPreviousValue(dv)
  }

  case class ColFuncColumnParameter(override val value: Funcs) extends ColumnParameter {
    type T = Funcs
    override val func: Funcs = value
  }

}

abstract class ColOp(val op: String, val param: String, val splline: Int) {
  def exec(): Unit

  def flush(): Unit

  val columnOperation: Any
}


abstract class ColOpFunction(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int) {

  import ColOp.logger

  def verify: PartialFunction[Ops, Unit => Unit]

  // ToDo: This whole function is UGLY and needs to be REMOVED
  protected def ColString(x: ColumnParameter): Option[String] = {
    x match {
      case y: StringColumnParameter => Option(y.getValue.toString)
      case l: LongColumnParameter => Option(l.getValue.toString)
      case d: DoubleColumnParameter => Option(d.getValue.toString)
      case z: ColColumnParameter =>
        z.getValue match {
          case sv: StringValue => Option(sv.value)
          case lv: LongValue => Option(lv.value.toString)
          case dv: DoubleValue => Option(dv.value.toString)
          case bv: BooleanValue => Option(bv.value.toString)
          case EmptyValue => None
        }
      case _ => None
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

trait ColOpTrait extends ColOp {

  import ColOp._

  val op: String
  val param: String
  val splline: Int

  override val columnOperation: Ops = Ops.withName(op)

  logger.debug(SIM.mpspath, s"ColOp: op = $op, param=$param, splline=$splline, table = ${COS.table_name}")

  private var colparam = Vector.empty[ColumnParameter]

  if (columnOperation != CONSTRAIN && columnOperation != COLCASE && columnOperation != COLEND && columnOperation != COLELSE) {
    val qp = Qsplitter(param)
    logger.debug(SIM.mpspath, s"Qsplitter result=$qp")
    for (p <- qp) {
      colParam(p.trim)
    }
  }
  logger.debug(SIM.mpspath, s"colparam=$colparam")

  private val colAddRowNumber = new ColAddRowNumber(colparam, op, param, splline)
  private val colAssert = new ColAssert(colparam, op, param, splline)
  private val colBound = new ColBound(colparam, op, param, splline)
  private val colCalc = new ColCalc(colparam, op, param, splline)
  private val colCase = new ColCase(colparam, op, param, splline)
  private val colCopy = new ColCopy(colparam, op, param, splline)
  private val colCount = new ColCount(colparam, op, param, splline)
  private val rowDrop = new RowDrop(colparam, op, param, splline)
  private val colDrop = new ColDrop(colparam, op, param, splline)
  private val colElse = new ColElse(colparam, op, param, splline)
  private val colEnd = new ColEnd(colparam, op, param, splline)
  private val colFill = new ColFill(colparam, op, param, splline)
  private val colHierarchy = new ColHierarchy(colparam, op, param, splline)
  private val colJoin = new ColJoin(colparam, op, param, splline)
  private val colLookupByName = new ColLookupByName(colparam, op, param, splline)
  private val colLookupByPosition = new ColLookupByPosition(colparam, op, param, splline)
  private val colMap = new ColMap(colparam, op, param, splline)
  private val colPrint = new ColPrint(colparam, op, param, splline)
  private val colRep = new ColRep(colparam, op, param, splline)
  private val colRowSplit = new ColRowSplit(colparam, op, param, splline)
  private val colSplit = new ColSplit(colparam, op, param, splline)
  private val colWhen = new ColWhen(colparam, op, param, splline)
  private val constrain = new Constrain(colparam, op, param, splline)

  // these are empty
  private val colBranch = new ColBranch(colparam, op, param, splline)
  private val colPush = new ColPush(colparam, op, param, splline)
  private val colFile = new ColFile(colparam, op, param, splline)

  // ToDo: The order of these partial functions should be in the order of maximum usage
  private val verify: PartialFunction[Ops, Unit => Unit] = (colCalc.verify orElse colRep.verify orElse colCopy.verify
    orElse colSplit.verify orElse colFill.verify  orElse constrain.verify orElse colMap.verify
    orElse colJoin.verify orElse colPrint.verify orElse colRowSplit.verify
    orElse colCase.verify orElse colElse.verify orElse colEnd.verify orElse colWhen.verify orElse colCount.verify
    orElse rowDrop.verify orElse colDrop.verify orElse colAssert.verify orElse colBound.verify
    orElse colAddRowNumber.verify orElse colHierarchy.verify orElse colLookupByName.verify
    orElse colLookupByPosition.verify orElse colBranch.verify orElse colPush.verify orElse colFile.verify )

    if (!verify.isDefinedAt(columnOperation))
    SIM.fatal(s"ColOp ($columnOperation) not understood, l# $splline")

  private val execute: Unit => Unit = verify(columnOperation)

  //////////// Constructor ends here //////////////

  private def dumpCols {
    if (COS.cols.size < 1)
      logger.error(SIM.mpspath, "No Columns", true)
    else {
      var Cols: String = COS.cols.head._1
      if (COS.cols.size > 1)
        for (c <- COS.cols.tail)
          Cols += ", " + c._1
      logger.debug(SIM.mpspath, s"Columns: $Cols")
    }
  }

  private def colParam(p: String) {
    if (p == null || p.isEmpty) {
      SIM.fatal(s"COL parameter empty, l# $splline")
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
        SIM.fatal(s"SPL regexes do not take Perl modifiers $p, l# $splline")
    } else if (p.head.isUpper) {
      // FUNC
      try {
        colparam = colparam :+ ColFuncColumnParameter(Funcs.withName(p))
      } catch {
        case e: java.util.NoSuchElementException =>
          SIM.error(s"Unknown COLCALC function: $p, l# $splline")
      }
    } else if (p.head.isLower) {
      // column
      COS.cols.get(p) match {
        case Some(c) =>
          colparam = colparam :+ ColColumnParameter(c)
        case None =>
          SIM.fatal(s"COL parameter: column $p not found, l# $splline")
          dumpCols
      }
    } else {
      SIM.fatal(s"COL parameter ($p) does not match Literal/Numeric/Pattern/Func/Column, l# $splline")
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

  override def flush(): Unit = {
    columnOperation match {
      case COLCOUNT => //sess_count = 0 For each Session
        val column: Column = colparam(2).column
        column.sess_count = 0
        column.setValue(LongValue(column.sess_count))
      case _ =>
    }
  }

  override def exec(): Unit = {
    try {
      logger.debug(SIM.mpspath, s"COLOP/exec ($columnOperation) got called for table = ${COS.table_name}, cases = ${COS.cases}")

      if ((COS.cases == NOCASE || COS.cases == CASETHEN) || (columnOperation == COLCASE || columnOperation == COLWHEN ||
        columnOperation == COLELSE || columnOperation == COLEND)) {
        execute(())
      }

    } catch {
      case NonFatal(e) =>
        logger.error(e, SIM.mpspath, s"COLOP/exec ($columnOperation) failed for table = ${COS.table_name})", true)
    }
  }
}
