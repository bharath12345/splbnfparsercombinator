package spl

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object TableMatcher extends RegexMatcher[TABLE] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val name = value_list.head
    val ns = value_list(1)
    val desc = Option(value_list(2))
    TABLE(name, ns, desc)
  }

  override protected val regex: Regex = """^DEFINE\s+TABLE\s+([\w_\{\}]+)\s+NAMESPACE\s+([\w\.\{\}]+)\s*(DESCRIPTION\s+'.+?'|)\s*""".r
}

object Linegrab extends RegexMatcher[LINEGRAB] {
  override protected val regex: Regex = """LINEGRAB\s+/(.*?)/$""".r

  override protected def get(values: String*): LINEGRAB = LINEGRAB(values.head.r)
}

object SetXmlNamespace extends RegexMatcher[SETXMLNAMESPACE] {
  override protected val regex: Regex = """SETXMLNAMESPACE\s*/\s*(.*)\s*/""".r

  override protected def get(values: String*): SETXMLNAMESPACE = {
    val urls: Array[String] = {
      if (!values.head.isEmpty) values.head.split(",") else Array.empty[String]
    }
    SETXMLNAMESPACE(urls)
  }
}

object AddContext extends RegexMatcher[ADDCONTEXT] {
  override protected val regex: Regex = """ADD_CONTEXT\s*\(([\w_ ,]+?)\)""".r

  override protected def get(values: String*): ADDCONTEXT = ADDCONTEXT(values.head)
}

object Multiline extends RegexMatcher[MULTILINE] {
  override protected val regex: Regex = """MULTILINE\s+(NOT|)\s*/(.+?)/\s*('.+?'|)""".r

  override protected def get(values: String*): MULTILINE = {
    val value_list = values.toList
    val not = (value_list.head == "NOT")
    val pat = new Regex(value_list(1))
    val sub = value_list(2)
    MULTILINE(not, pat, sub)
  }
}

object MultilineBreakOnUnmatch extends RegexMatcher[MULTILINE_BREAK_ON_UNMATCH] {
  override protected val regex: Regex = """MULTILINE_BREAK_ON_UNMATCH\s+/(.+?)/\s*('.+?'|)""".r

  override protected def get(values: String*): MULTILINE_BREAK_ON_UNMATCH = {
    val value_list = values.toList
    val pat = new Regex(value_list(0))
    val sub = value_list(1)
    MULTILINE_BREAK_ON_UNMATCH(pat, sub)
  }
}

object Skip extends RegexMatcher[SKIP] {
  override protected val regex: Regex = """SKIP\s+(\d+)""".r

  override protected def get(values: String*): SKIP = SKIP(values.head.toInt)
}