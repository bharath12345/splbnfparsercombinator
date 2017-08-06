package spl.lexer

import spl.colops.Ops
import spl.colops.Ops.Ops

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
    COLUMN(null, name, aspect, ddl, attribs, as, align, solrmap, kafka)
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
  override type T = COLOP

  override protected val regex: Regex = """(COL\w+)\s*\((.*?)\)""".r

  override protected def get(values: String*): COLOP = {
    val value_list = values.toList
    val op: Ops = Ops.withName(value_list.head)
    COLOP(op, value_list(1))
  }
}

object RowOpLexer extends RegexLexer {
  override type T = ROWOP

  override protected val regex: Regex = """(ROW\w+)\s*\((.*?)\)""".r

  override protected def get(values: String*): ROWOP = {
    val value_list = values.toList
    ROWOP(value_list(0), value_list(1))
  }
}

object ColCaseOpsLexer extends RegexLexer {
  override type T = COPCASEOP

  override protected val regex: Regex = """(COLCASE|COLELSE|COLEND)""".r

  override protected def get(values: String*): COPCASEOP = COPCASEOP(Ops.withName(values.head))
}
