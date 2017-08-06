package com.glassbeam.scalar.core.colops

import java.text.SimpleDateFormat
import java.time.temporal.{ChronoUnit, Temporal}
import java.time.{Instant, Period, ZoneId}
import java.util.Calendar._
import java.util.{Calendar, Date, Random, TimeZone}

import com.glassbeam.scalar.core.dateformat.DateFormats
import com.glassbeam.scalar.core.parser.Funcs._
import com.glassbeam.scalar.core.parser.Ops._
import ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model._
import com.glassbeam.scalar.model.constants.Constants
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time.{DateTime, DateTimeZone}

import scala.collection.immutable.{HashMap, Vector}
import scala.math._
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColCalc extends Logger {
  private final lazy val logger = Logging(this)
}

class ColCalc(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) with NumberSystem with Constants {

  import ColCalc._

  //Note: creating a new SimpleDateFormat pattern compiles it. thats why these caches are useful. Look into the code of
  // Java SimpleDateFormat - in its constructor it calles the initialize() method which caches a compiled pattern to be
  // applied on invoke of parse
  private var sdf2epoch_sdfMap = HashMap[String, SimpleDateFormat]()
  private var epoch2sdf_sdfMap = HashMap[String, SimpleDateFormat]()
  private var adjyear_sdfMap = HashMap[String, SimpleDateFormat]()
  private var datediff_between: Option[(Temporal, Temporal) => Long] = None
  
  val dateFormat = new DateFormats

  private lazy val cal = Calendar.getInstance(TIMEZONE_UTC) // Instantiate once

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // FN: ADJYEAR -- COLCALC(destCol, ADJYEAR, 'Apr', '2009', 'Dec')
    // FN: GMTIME
    // FN: LOCALTIME
    case COLCALC =>
      var colerror = false
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        SM.error(s"COLCALC first parameter must be a COLUMN, l# $splline")
        colerror = true
      }
      var func: Funcs = InvalidFunc
      try {
        func = colparam(1).func
      } catch {
        case e: java.lang.ClassCastException =>
          SM.fatal(s"COLCALC Function does not exist, l# $splline")
          colerror = true
      }
      func match {
        case ZEROPAD =>
          val align = ColString(colparam(2))
          if (align.head != "L" && align.head != "R") {
            SM.error(s"COLCALC/ZEROPAD must specify L or R, l# $splline")
            colerror = true
          }

        case STR2TIME =>
          SM.fatal(s"STR2TIME($splline) NOT SUPPORTED -- use SDF2EPOCH")
          colerror = true

        case STR2MMYY =>
          SM.fatal(s"STR2MMYY($splline) NOT SUPPORTED -- use SDF2EPOCH & TIME2MONTH")
          colerror = true

        case InvalidFunc =>
          SM.fatal(s"COLCALC Function does not exist, l# $splline")
          colerror = true

        case SDF2EPOCH =>
          if (colparam.size < 4) {
            SM.error(s"COLCALC must have at least four parameters, l# $splline")
            colerror = true
          } else {
            ColString(colparam(2)) match {
              case Some(fmt) =>
                logger.debug(SM.mpspath, s"Compiling SDF, Format=${fmt}")
                val sdf = new SimpleDateFormat(fmt)
                sdf.setTimeZone(TIMEZONE_UTC)
                sdf2epoch_sdfMap += (fmt -> sdf)
              case None =>
                SM.error(s"SDF2EPOCH format should not be null, l# $splline")
                colerror = true
            }
          }

        case EPOCH2SDF =>
          try {
            logger.debug(SM.mpspath, s"EPOCH2SDF($splline) input=${ColString(colparam(3))} format=${ColString(colparam(2))}")
            for {fmt <- ColString(colparam(2))} {
              val sdf = new SimpleDateFormat(fmt)
              val timezone = if (colparam.length > 4) ColString(colparam(4)) else None
              if (timezone.isDefined) sdf.setTimeZone(TIMEZONE_UTC)
              epoch2sdf_sdfMap += (fmt -> sdf)
            }
          } catch {
            case NonFatal(e) =>
              SM.error(warningString("EPOCH2SDF", Option(e)))
              colerror = true
          }

        case DATEDIFF =>
          if(colparam.size != 5){
            SM.error(s"COLCALC must have at least five parameters, l# $splline")
            colerror = true
          } else {
            val requiredFormat = ColString(colparam(4))
            if (requiredFormat.isEmpty) {
              SM.error(s"Incorrect parameters. Empty required format $requiredFormat, l# $splline")
              colerror = true
            } else {
              logger.debug(s"Rrequired format $requiredFormat")
              datediff_between = requiredFormat.map {
                case "s" => ChronoUnit.SECONDS.between _
                case "m" => ChronoUnit.MINUTES.between _
                case "h" => ChronoUnit.HOURS.between _
                case "d" => ChronoUnit.DAYS.between _
                case "y" => ChronoUnit.YEARS.between _
                case _ =>
                  SM.error(s"Incorrect required format $requiredFormat, l# $splline")
                  colerror = true
                  null
              }
            }
          }

        case ADJYEAR =>
          try {
            // COLCALC(destCol, ADJYEAR, obs_epoch, evt_date_str, evt_date_sdf, offset, timezone)
            for {
              c <- ColString(colparam(4))
              e <- ColString(colparam(6))
            } {
              val fmt = c.trim
              val timezone = e.trim

              val sdf = new SimpleDateFormat(fmt)
              if (timezone.nonEmpty)
                sdf.setTimeZone(TimeZone.getTimeZone(timezone))
              else
                sdf.setTimeZone(TIMEZONE_UTC)

              adjyear_sdfMap += (fmt -> sdf)
            }
          } catch {
            case NonFatal(ex) =>
              SM.error(warningString("ADJYEAR", Option(ex)))
              colerror = true
          }

        case _ =>
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      val func: Funcs = colparam(1).func
      logger.debug(SM.mpspath, "COLCALC(" + splline + ") function Value = " + func)
      func match {
        case STR2TIME =>
          try {
            SM.warning(s"STR2TIME logline=$SM.lineno NOT SUPPORTED -- use SDF2EPOCH")
            ColString(colparam(2)).foreach { value =>
              colparam.head.setValue(LongValue(dateFormat.parse(value)))
            }
          } catch {
            case NonFatal(e) =>
          }

        case SDF2EPOCH =>
          try {
            logger.debug(SM.mpspath, s"SDF2EPOCH($splline) input=${ColString(colparam(3))}")
            for { x <- ColString(colparam(2)); y <- ColString(colparam(3)) }
              colparam.head.setValue(LongValue(sdf2epoch_sdfMap(x).parse(y).getTime))
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("SDF2EPOCH", Option(e)))
          }

        case DATEDIFF =>
          try {
              val startDateInMillSecs = dateFormat.parse(ColString(colparam(2)).get)
              val endDateInMillSecs = dateFormat.parse(ColString(colparam(3)).get)
              if(endDateInMillSecs > startDateInMillSecs){
                val date1 = Instant.ofEpochMilli(startDateInMillSecs).atZone(ZoneId.systemDefault()).toLocalDateTime
                val date2 = Instant.ofEpochMilli(endDateInMillSecs).atZone(ZoneId.systemDefault()).toLocalDateTime
                val diff: Long = datediff_between match {
                  case Some(function) => function(date1, date2)
                  case None => 0L
                }
                colparam.head.setValue(LongValue(diff))
                logger.debug(s"Recieved start date $startDateInMillSecs end date $endDateInMillSecs, diff = $diff")
              } else {
                logger.warning(s"start date can't be greater than the end date. Recieved start date $startDateInMillSecs, end date $endDateInMillSecs")
              }

            }catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("DATEDIFF", Option(e)))
          }

        case EPOCH2SDF =>
          try {
            logger.debug(SM.mpspath, s"EPOCH2SDF($splline) input=${ColString(colparam(3))} format=${ColString(colparam(2))}")
            for {
              x <- ColString(colparam(2))
              y <- ColString(colparam(3))
            } {
              val sdf = epoch2sdf_sdfMap(x)
              val value = y.toLong
              // We need epoch value in milliseconds, but may also get in seconds. For a faster way to differentiate,
              // compare against a 1970's epoch value (consider the maximum digits which definitely belong to the 1970's, i.e. 99999999
              val MAX_1970_EPOCH = 99999999L
              val epoch = if (value / (MAX_1970_EPOCH * 1000) <= 0) value * 1000 else value
              val date = new Date(epoch)
              colparam.head.setValue(StringValue(sdf.format(date)))
              logger.debug(SM.mpspath, s"EPOCH2SDF date($date), sdf(${ColString(colparam(2))}), newdate(${sdf.format(date)})")
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("EPOCH2SDF", Option(e)))
          }

        case STR2MMYY => // Deprecated
          SM.warning(s"STR2MMYY($splline) NOT SUPPORTED -- use SDF2TIME & TIME2MONTH")
          try {
            ColString(colparam(2)).foreach { src =>
              val x = dateFormat.parse(src)
              cal.setTimeInMillis(x.toLong)
              cal.set(DAY_OF_MONTH, 1) // What is te EPOCH of the first day of the month
              cal.set(HOUR_OF_DAY, 0)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              colparam.head.setValue(LongValue(cal.getTimeInMillis))
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("STR2MMYY", Option(e)))
          }

        case TIME2QTR =>
          try {
            logger.debug(SM.mpspath, "TIME2QTR(" + splline + ")")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(Calendar.MONTH, cal.get(Calendar.MONTH) / 4 * 4)
              cal.set(HOUR_OF_DAY, 0)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv =   LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME2QTR", Option(e)))
          }

        case TIME2MONTH =>
          try {
            ColString(colparam(2)).foreach { src =>
              logger.debug(SM.mpspath, "TIME2MTH(" + splline + ")")
              cal.setTimeInMillis(src.toLong)
              cal.set(DAY_OF_MONTH, 1) // What is the EPOCH of the first day of the month
              cal.set(HOUR_OF_DAY, 0)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv = LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME2MONTH", Option(e)))
          }

        case TIME2WEEK =>
          try {
            ColString(colparam(2)).foreach { src =>
              logger.debug(SM.mpspath, "TIME2WEEK")
              cal.setTimeInMillis(src.toLong)
              cal.set(DAY_OF_WEEK, SUNDAY)
              cal.set(HOUR_OF_DAY, 0)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv = LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME2WEEK", Option(e)))
          }

        case TIME2DAY =>
          try {
            logger.debug(SM.mpspath, "TIME2DAY")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(HOUR_OF_DAY, 0)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv = LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME2DAY", Option(e)))
          }

        case TIME230DAY =>
          try {
            logger.debug(SM.mpspath, "TIME230DAY")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(DAY_OF_YEAR, cal.get(DAY_OF_YEAR) / 30 * 30)
              cal.set(HOUR_OF_DAY, 0)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv =LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME230DAY", Option(e)))
          }

        case TIME2HOUR =>
          try {
            logger.debug(SM.mpspath, "TIME2HOUR")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv =LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME2HOUR", Option(e)))
          }

        case TIME210MIN =>
          try {
            logger.debug(SM.mpspath, "TIME210MIN")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(MINUTE, cal.get(MINUTE) / 10 * 10)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv =LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME210MIN", Option(e)))
          }

        case TIME2MIN =>
          try {
            logger.debug(SM.mpspath, "TIME2MIN")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv =LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME2MIN", Option(e)))
          }

        case TIME24HOUR =>
          try {
            logger.debug(SM.mpspath, "TIME210MIN")
            ColString(colparam(2)).foreach { src =>
              cal.setTimeInMillis(src.toLong)
              cal.set(HOUR_OF_DAY, cal.get(HOUR_OF_DAY) / 4 * 4)
              cal.set(MINUTE, 0)
              cal.set(SECOND, 0)
              cal.set(MILLISECOND, 0)
              val newv =LongValue(cal.getTimeInMillis)
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("TIME24HOUR", Option(e)))
          }

        case ADJYEAR =>
          try {
            // COLCALC(destCol, ADJYEAR, obs_epoch, evt_date_str, evt_date_sdf, offset, timezone)
            for {
              a <- ColString(colparam(2))
              b <- ColString(colparam(3))
              c <- ColString(colparam(4))
              d <- ColString(colparam(5))
            } {
              val obs_epoch = NumericLong(a)
              val evt_date_str = b.trim
              val fmt = c.trim
              val offset = NumericLong(d)
              var adjyear = 0

              if (obs_epoch == 0 || evt_date_str.isEmpty)
                logger.warning(SM.mpspath, warningString("ADJYEAR: Either epoch or date string is empty", None))
              else {

                // Get obs year, month and date
                val obs_ddmmyy = new DateTime(obs_epoch, DateTimeZone.UTC)
                val obs_year = obs_ddmmyy.getYear
                val obs_month = obs_ddmmyy.getMonthOfYear //Starts from 1
                val obs_day = obs_ddmmyy.getDayOfMonth
                adjyear = obs_year

                // Get event month and date. Add timezone if present
                val sdf = adjyear_sdfMap(fmt)
                val temp_evt_date = sdf.parse(evt_date_str)
                val evt_date = new DateTime(temp_evt_date).withZone(DateTimeZone.UTC)
                // Converting to joda time because java time date setting was giving weird results
                val evt_month = evt_date.getMonthOfYear // Starts from 1
                val evt_day = evt_date.getDayOfMonth //evt_date.getDate

                // First adjyear calculation to determine if year is one less
                if ((obs_month < evt_month) || (obs_month == evt_month && obs_day < evt_day))
                  adjyear = obs_year - 1

                // Add adjyear to event date, if it doesnt have year, to get the epoch for comparision against offset
                val new_evt_date = evt_date.withYear(adjyear)
                val evt_epoch: Long = new_evt_date.getMillis
                if (evt_epoch > (obs_epoch + offset))
                  adjyear -= 1

              }
              if (adjyear <= 0)
                adjyear = 1970
              colparam.head.setValue(LongValue(adjyear))
            }
          } catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("ADJYEAR", Option(ex)))
          }

        case PLUS =>
          try {
            var dst: Double = 0
            val dstCol = colparam.head.column
            val dstDdl = dstCol.typ.toString + dstCol.len.toString
            for {
              m <- colparam.drop(2)
              n <- ColString(m)
            } {
              dst += NumericDouble(n)
            }
            dstCol.setValue(numericPerColtype(dstDdl, dst))
          } catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("PLUS", Option(ex)))
          }

        case MINUS =>
          try {
            ColString(colparam(2)).foreach { src =>
              var dst: Double = NumericDouble(src)
              val dstCol = colparam.head.column
              val dstDdl = dstCol.typ.toString + dstCol.len.toString
              for {m <- colparam.drop(3) ; n <- ColString(m)} {
                dst -= NumericDouble(n)
              }
              dstCol.setValue(numericPerColtype(dstDdl, dst))
            }
          } catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("MINUS", Option(ex)))
          }

        case TIMES =>
          try {
            ColString(colparam(2)).foreach { src =>
              var dst: Double = NumericDouble(src)
              val dstCol = colparam.head.column
              val dstDdl = dstCol.typ.toString + dstCol.len.toString
              for {m <- colparam.drop(3) ; n <- ColString(m)}
                dst *= NumericDouble(n)
              dstCol.setValue(numericPerColtype(dstDdl, dst))
            }
          }catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("TIMES", Option(ex)))
          }

        case DIVIDEBY =>
          try {
            ColString(colparam(2)).foreach { src =>
              logger.debug(SM.mpspath, s"DIVIDEBY ${ColString(colparam(2))}")
              var dst: Double = NumericDouble(src)
              val dstCol = colparam.head.column
              val dstDdl = dstCol.typ.toString + dstCol.len.toString
              if (dst == 0) {
                val msg = "Divide By Zero Error"
                logger.warning(SM.mpspath, warningString(msg, None))
              } else {
                for {
                  x <- colparam.drop(3)
                  y <- ColString(x)
                } {
                  dst /= y.toDouble
                }
                dstCol.setValue(numericPerColtype(dstDdl, dst))
              }
            }
          }catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("DIVIDEBY", Option(ex)))
          }

        case CONCAT =>
          var dst: String = ""
          for { x <- colparam.drop(2); y <- ColString(x)} {
            dst += y
          }
          colparam.head.setValue(StringValue(dst))

        case XTOPOWY =>
          try {
            if (ColString(colparam(2)).isEmpty || ColString(colparam(3)).isEmpty) {
              val msg = "XTOPOWY: X or Y is empty. X=" + ColString(colparam(2)) + ", Y=" + ColString(colparam(3))
              logger.warning(SM.mpspath, warningString(msg, None))
            } else {
              for {
                x <- ColString(colparam(2))
                y <- ColString(colparam(3))
              } {
                val dst: Double = pow(NumericDouble(x), NumericDouble(y))
                val newv =
                  if (colparam.head.typ == ColumnType.INTEGER || colparam.head.typ == ColumnType.LONG)
                    LongValue(dst.toLong)
                  else
                    DoubleValue(dst)
                colparam.head.setValue(newv)
              }
            }
          }catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("XTOPOWY", Option(ex)))
          }

        case HEX2DEC =>
          try {
            ColString(colparam(2)).foreach { src =>
              val col = colparam.head.column
              val x = col.len.toString() match {
                case "32" => Integer.parseInt(src, 16)
                case _ => java.lang.Long.parseLong(src, 16)
              }
              colparam.head.setValue(LongValue(x))
            }
          }catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("HEX2DEC", Option(ex)))
          }

        case INT =>
          try {
            ColString(colparam(2)).foreach { src =>
              colparam.head.setValue(LongValue(NumericDouble(src).round))
            }
          }catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("INT", Option(ex)))
          }

        case STR2SUM =>
          try {
            var value: Long = 0
            for {
              u <- ColString(colparam(2))
              v <- u.split(' ')
            } value += v.toLong
            colparam.head.setValue(LongValue(value))
          } catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("STR2SUM", Option(ex)))
          }

        case LENGTH =>
          try {
            ColString(colparam(2)).foreach { src =>
              colparam.head.setValue(LongValue(src.length))
            }
          } catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("LENGTH", Option(ex)))
          }

        case UC =>
          try {
            ColString(colparam(2)).foreach { src =>
              colparam.head.setValue(StringValue(src.toUpperCase))
            }
          } catch {
            case NonFatal(ex) =>
              logger.warning(SM.mpspath, warningString("UC", Option(ex)))
          }

        case LC =>
          ColString(colparam(2)).foreach { src =>
            colparam.head.setValue(StringValue(src.toLowerCase))
          }
        case MD5 =>
          try {
            ColString(colparam(2)).foreach { src =>
              colparam.head.setValue(StringValue(DigestUtils.md5Hex(src))) //akka.util.Crypt.md5(src)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("MD5", Option(e)))
          }

        case ZEROPAD =>
          try {
            for {
              src <- ColString(colparam(4))
              dst <- ColString(colparam(3))
              hhh <- ColString(colparam(2))
            } {
              val pad = dst.toInt - src.length
              val newv =
                if (hhh.head == 'L') StringValue(("0" * pad) + src)
                else StringValue(src + ("0" * pad))
              colparam.head.setValue(newv)
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("ZEROPAD", Option(e)))
          }

        case RANDINT =>
          colparam.head.setValue(LongValue((new Random).nextLong()))

        // Range:Long
        case HEX2BIN =>
          try {
            ColString(colparam(2)).foreach { src =>
              colparam.head.setValue(StringValue(hexToBinary(src)))
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("HEX2BIN", Option(e)))
          }

        case BIN2HEX =>
          try {
            ColString(colparam(2)).foreach { src =>
              colparam.head.setValue(StringValue(binaryToHex(src)))
            }
          } catch {
            case NonFatal(e) =>
              logger.warning(SM.mpspath, warningString("BIN2HEX", Option(e)))
          }

        case _ =>
          SM.warning(s"COLCALC function $func not yet implemented, l# $splline")
      }
  }
}