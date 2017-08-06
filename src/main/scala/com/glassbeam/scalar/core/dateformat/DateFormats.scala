package com.glassbeam.scalar.core.dateformat

import java.text.SimpleDateFormat
import java.util.Calendar

import com.glassbeam.scalar.model.Logger

class DateFormats extends Logger {

  private final lazy val logger = Logging(this)

  // "Wed, 09 Feb 1994 22:23:32 GMT"       -- HTTP format
  lazy val pat01_r =
    """\w\w\w, \d\d \w\w\w \d\d\d\d \d\d\:\d\d\:\d\d \w\w\w[\d:+-]*""".r
  lazy val sdf01 = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z")

  // "Thu Feb  03 17:03:55 GMT 1994"        -- ctime(3) format
  lazy val pat02_r =
    """\w\w\w \w\w\w \d\d \d\d\:\d\d\:\d\d \w\w\w \d\d\d\d""".r
  lazy val sdf02 = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy")

  //  "Thu Feb  03 00:00:00 1994",           -- ANSI C asctime() format
  lazy val pat03_r =
    """ \w\w\w \w\w\w \d\d \d\d\:\d\d\:\d\d \d\d\d\d""".r
  lazy val sdf03 = new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy")

  // "Tuesday, 08-Feb-94 14:15:29 GMT"     -- old rfc850 HTTP format
  lazy val pat04_r =
    """\w+, \d\d-\w\w\w-\d\d \d\d\:\d\d\:\d\d \w\w\w[d:+-]*""".r
  lazy val sdf04 = new SimpleDateFormat("E+, dd-MMM-yy HH:mm:ss z")

  // "Tuesday, 08-Feb-1994 14:15:29 GMT"   -- broken rfc850 HTTP format
  lazy val pat05_r =
    """\w+, \d\d\-\w\w\w\-\d\d\d\d \d\d\:\d\d\:\d\d \w\w\w[\d:+-]*""".r
  lazy val sdf05 = new SimpleDateFormat("E+, dd-MMM-yyyy HH:mm:ss z")

  // "03/Feb/1994:17:03:55 -0700"   -- common logfile format
  lazy val pat06_r =
    """\d\d\/\w\w\w\/\d\d\d\d\:\d\d\:\d\d\:\d\d [+-]\d\d\d\d""".r
  lazy val sdf06 = new SimpleDateFormat("dd/MMM/yyyy:HH:mm:ss Z")

  // "09 Feb 1994 22:23:32 GMT"     -- HTTP format (no weekday)
  lazy val pat07_r =
    """\d\d \w\w\w \d\d\d\d \d\d\:\d\d\:\d\d \w\w\w[\d:+-]*""".r
  lazy val sdf07 = new SimpleDateFormat("dd MMM yyyy HH:mm:ss z")

  // "08-Feb-94 14:15:29 GMT"       -- rfc850 format (no weekday)
  lazy val pat08_r =
    """\d\d\-\w\w\w\-\d\d \d\d\:\d\d\:\d\d \w\w\w[\d:+-]*""".r
  lazy val sdf08 = new SimpleDateFormat("dd-MMM-yy HH:mm:ss z")

  // "08-Feb-1994 14:15:29 GMT"     -- broken rfc850 format (no weekday)
  lazy val pat09_r =
    """\d\d\-\w\w\w\-\d\d\d\d \d\d\:\d\d\:\d\d \w\w\w[\d:+-]*""".r
  lazy val sdf09 = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss z")

  // "1994-02-03 14:15:29 -0100"    -- ISO 8601 format
  lazy val pat10_r =
    """\d\d\d\d\-\d\d\-\d\d \d\d\:\d\d\:\d\d [+-]\d\d\d\d""".r
  lazy val sdf10 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z")

  // Polycom   21-11-2011 21-02-54
  lazy val pat11_r =
    """\d\d\-\d\d\-\d\d\d\d \d\d\-\d\d\-\d\d""".r
  lazy val sdf11 = new SimpleDateFormat("dd-MM-yyyy HH-mm-ss")

  // "1994-02-03 14:15:29"          -- zone is optional
  lazy val pat12_r =
    """\d\d\d\d\-\d\d\-\d\d \d\d\:\d\d\:\d\d""".r
  lazy val sdf12 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  //  "1994-02-03"                   -- only date
  lazy val pat13_r =
    """\d\d\d\d\-\d\d\-\d\d""".r
  lazy val sdf13 = new SimpleDateFormat("yyyy-MM-dd")

  // "1994-02-03T14:15:29"          -- Use T as separator
  lazy val pat14_r =
    """\d\d\d\d\-\d\d\-\d\d\w\d\d\:\d\d\:\d\d""".r
  lazy val sdf14 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

  // "19940203T141529Z"             -- ISO 8601 compact format
  lazy val pat15_r =
    """\d{8}\w\d{6}\S+""".r
  lazy val sdf15 = new SimpleDateFormat("yyyyMMdd'T'HHmmssZ")

  // "19940203"                     -- only date
  lazy val pat16_r =
    """\d{8}""".r
  lazy val sdf16 = new SimpleDateFormat("yyyyMMdd")

  // "08-Feb-94"         -- old rfc850 HTTP format    (no weekday, no time)
  lazy val pat17_r =
    """\d\d\-\w\w\w\-\d\d""".r
  lazy val sdf17 = new SimpleDateFormat("dd-MMM-yy")

  // "1994-02-03T14:15:29.299"          -- Use T as separator
  lazy val pat18_r =
    """\d\d\d\d\-\d\d\-\d\d\w\d\d\:\d\d\:\d\d\.\d\d\d""".r
  lazy val sdf18 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")

  lazy val cal = Calendar.getInstance // Instantiate once

  def parse(x: String): Long = {
    try {
      x match {
        case pat01_r() =>
          cal.setTime(sdf01.parse(x))
        case pat02_r() =>
          cal.setTime(sdf02.parse(x))
        case pat03_r() =>
          cal.setTime(sdf03.parse(x))
        case pat04_r() =>
          cal.setTime(sdf04.parse(x))
        case pat05_r() =>
          cal.setTime(sdf05.parse(x))
        case pat06_r() =>
          cal.setTime(sdf06.parse(x))
        case pat07_r() =>
          cal.setTime(sdf07.parse(x))
        case pat08_r() =>
          cal.setTime(sdf08.parse(x))
        case pat09_r() =>
          cal.setTime(sdf09.parse(x))
        case pat10_r() =>
          cal.setTime(sdf10.parse(x))
        case pat11_r() =>
          cal.setTime(sdf11.parse(x))
        case pat12_r() =>
          cal.setTime(sdf12.parse(x))
        case pat13_r() =>
          cal.setTime(sdf13.parse(x))
        case pat14_r() =>
          cal.setTime(sdf14.parse(x))
        case pat15_r() =>
          cal.setTime(sdf15.parse(x))
        case pat16_r() =>
          cal.setTime(sdf16.parse(x))
        case pat17_r() =>
          cal.setTime(sdf17.parse(x))
        case pat18_r() =>
          cal.setTime(sdf18.parse(x))
        /*
      case pat16 =>
        cal.setTime(sdf16.parse(x))
      case pat17 =>
        cal.setTime(sdf17.parse(x))
      case pat18 =>
        cal.setTime(sdf18.parse(x))
      case pat19 =>
        cal.setTime(sdf19.parse(x))
      case pat20 =>
        cal.setTime(sdf20.parse(x))
      case pat21 =>
        cal.setTime(sdf21.parse(x))
        */

      }
      cal.getTimeInMillis
    } catch {
      case e: Exception =>
        logger.error(e, logger.NoMPS, s"Date pattern ($x) not found STR2TIME", true)
        throw e
    }
  }
}

