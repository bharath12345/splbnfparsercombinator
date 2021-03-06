package com.glassbeam.scalar.core.parser.spl.lexer

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object Icon1Lexer extends RegexLexer {

  type T = ICON1

  override protected def get(values: String*) = ICON1(Icons.withName(values.head.toUpperCase))

  override protected val regex: Regex = """ICON\s+(list\_basic|syslog|apache)""".r
}

object Icon2Lexer extends RegexLexer {

  type T = ICON2

  override protected def get(values: String*) = {
    val value_list = values.toList
    val separator: Regex = value_list.head.r
    val delimiter: Regex = value_list(1).r
    ICON2(Icons.NVPAIR_BASIC, separator, delimiter)
  }

  override protected val regex: Regex = """ICON\s+nvpair_basic\s*(?:/(.*?)/\s*/(.*?)/|)""".r
}

object Icon3Lexer extends RegexLexer {

  type T = ICON3

  override protected def get(values: String*) = ICON3(Icons.NVPAIR_UNORDERED, values.head.r)

  override protected val regex: Regex = """ICON\s+nvpair_unordered\s*(?:/(.*?)/\s*|)""".r
}

object Icon4Lexer extends RegexLexer {

  type T = ICON4

  override protected def get(values: String*) = ICON4(Icons.XML_BASIC, values.head)

  override protected val regex: Regex = """ICON xml_basic\s*/\s*(.*)\s*/\s*""".r
}

object Icon5Lexer extends RegexLexer {

  type T = ICON5

  override protected def get(values: String*) = ICON5(Icons.JSON, values.head)

  override protected val regex: Regex = """ICON\s+json\s*/(.*)/""".r
}

object Icon6Lexer extends RegexLexer {

  type T = ICON6

  override protected def get(values: String*) = {
    val value_list = values.toList
    val horSep: String = value_list.head
    val verSep: String = value_list(1)
    val multilineKey = value_list(2)
    ICON6(Icons.ALIGNED_BASIC, horSep, verSep, multilineKey)
  }

  override protected val regex: Regex = """ICON\s+aligned_basic\s+'(.*?)'\s*,\s*'(.*?)'\s*,\s*'(.*?)'""".r
}

object Icon8Lexer extends RegexLexer {

  type T = ICON8

  override protected def get(values: String*) = ICON8(Icons.APACHE, values.head)

  override protected val regex: Regex = """ICON\s+apache\s*/(.*)/""".r
}

object Icon9Lexer extends RegexLexer {

  type T = ICON9

  override protected def get(values: String*) = {
    val value_list = values.toList
    val icon = Icons.withName(value_list.head.toUpperCase())
    val csvType: String = value_list(1)
    val sep: String = value_list(2)
    ICON9(icon, csvType, sep)
  }

  override protected val regex: Regex = """ICON\s+(csv_noheader|csv_withheader)\s*(\/.\/|)""".r
}