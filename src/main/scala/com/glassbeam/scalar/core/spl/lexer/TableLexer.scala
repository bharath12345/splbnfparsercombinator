package com.glassbeam.scalar.core.spl.lexer


import com.glassbeam.scalar.core.parser.{ColumnOps, RowOps}
import com.glassbeam.scalar.core.parser.ColumnOps.ColumnOps
import com.glassbeam.scalar.core.parser.RowOps.RowOps

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object TableLexer extends RegexLexer {

  type T = TABLE

  override protected def get(values: String*) = {
    val value_list = values.toList
    val name = value_list.head
    val ns = value_list(1)
    val desc = Option(value_list(2))
    TABLE(name, ns, desc)
  }

  override protected val regex: Regex = """^DEFINE\s+TABLE\s+([\w_\{\}]+)\s+NAMESPACE\s+([\w\.\{\}]+)\s*(DESCRIPTION\s+'.+?'|)\s*""".r
}

object ColumnLexer extends RegexLexer {
  type T = COLUMN

  override protected val regex: Regex = ("""^COLUMN\s+([\w_]+)\s+(\([\w\.\:]+?\)|)\s*\[(.+?)\]\s*(<.+?>|)\s*""" +
    """(AS\s+[\d\w_]+|AS\s+'.+?'|AS\s+'.+?'\s*WITH\s+NAME\s*'.+?'|)\s*(\[[LCR]\]|)""" +
    """\s*(WITH\s+SOLRMAPPING\s+\(.+?\)|)\s*(KAFKA)?""").r

  override protected def get(values: String*): COLUMN = {
    val value_list = values.toList
    val name: String = value_list.head
    val aspect: Option[String] = Option(value_list(1))
    val ddl: Option[String] = Option(value_list(2))
    val attribs: String = value_list(3)
    val as: Option[String] = Option(value_list(4))
    val align: Option[String] = Option(value_list(5))
    val solrmap: Option[String] = Option(value_list(6))
    val kafka: Boolean = value_list(7) == "KAFKA"
    COLUMN(name, aspect, ddl, attribs, as, align, solrmap, kafka)
  }
}

object LinegrabLexer extends RegexLexer {
  type T = LINEGRAB

  override protected val regex: Regex = """LINEGRAB\s+/(.*?)/$""".r

  override protected def get(values: String*): LINEGRAB = LINEGRAB(values.head.r)
}

object SetXmlNamespaceLexer extends RegexLexer {
  type T = SETXMLNAMESPACE

  override protected val regex: Regex = """SETXMLNAMESPACE\s*/\s*(.*)\s*/""".r

  override protected def get(values: String*): SETXMLNAMESPACE = {
    val urls: Array[String] = {
      if (!values.head.isEmpty) values.head.split(",") else Array.empty[String]
    }
    SETXMLNAMESPACE(urls)
  }
}

object AddContextLexer extends RegexLexer {
  type T = ADDCONTEXT

  override protected val regex: Regex = """ADD_CONTEXT\s*\(([\w_ ,]+?)\)""".r

  override protected def get(values: String*): ADDCONTEXT = ADDCONTEXT(values.head)
}

object MultilineLexer extends RegexLexer {
  type T = MULTILINE

  override protected val regex: Regex = """MULTILINE\s+(NOT|)\s*/(.+?)/\s*('.+?'|)""".r

  override protected def get(values: String*): MULTILINE = {
    val value_list = values.toList
    val not = (value_list.head == "NOT")
    val pat = new Regex(value_list(1))
    val sub = value_list(2)
    MULTILINE(not, pat, sub)
  }
}

object MultilineBreakOnUnmatchLexer extends RegexLexer {
  type T = MULTILINE_BREAK_ON_UNMATCH

  override protected val regex: Regex = """MULTILINE_BREAK_ON_UNMATCH\s+/(.+?)/\s*('.+?'|)""".r

  override protected def get(values: String*): MULTILINE_BREAK_ON_UNMATCH = {
    val value_list = values.toList
    val pat = new Regex(value_list(0))
    val sub = value_list(1)
    MULTILINE_BREAK_ON_UNMATCH(pat, sub)
  }
}

object SkipLexer extends RegexLexer {
  type T = SKIP

  override protected val regex: Regex = """SKIP\s+(\d+)""".r

  override protected def get(values: String*): SKIP = SKIP(values.head.toInt)
}

object ColOpLexer extends RegexLexer {
  override type T = COLUMNOPERATION

  override protected val regex: Regex = """(COL\w+)\s*\((.*?)\)""".r

  override protected def get(values: String*): COLUMNOPERATION = {
    val value_list = values.toList
    val op: ColumnOps = ColumnOps.withName(value_list.head)
    COLUMNOPERATION(op, value_list(1), value_list(2).toInt)
  }
}

object RowOpLexer extends RegexLexer {
  override type T = ROWOPERATION

  override protected val regex: Regex = """(ROW\w+)\s*\((.*?)\)""".r

  override protected def get(values: String*): ROWOPERATION = {
    val value_list = values.toList
    val op: RowOps = RowOps.withName(value_list.head)
    ROWOPERATION(op, value_list(1), value_list(2).toInt)
  }
}

object ColCaseOpsLexer extends RegexLexer {
  override type T = COLCASEOPERATION

  override protected val regex: Regex = """(COLCASE|COLELSE|COLEND)""".r

  override protected def get(values: String*): COLCASEOPERATION = {
    val value_list = values.toList
    COLCASEOPERATION(ColumnOps.withName(values.head), value_list(1).toInt)
  }
}
