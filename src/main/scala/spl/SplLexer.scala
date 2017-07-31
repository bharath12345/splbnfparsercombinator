package spl

import spl.NamespaceType.NamespaceType

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait SPL

case class SPL_ERROR(error: String) extends SPL
case class TABLE(name: String, namespace: String, desc: Option[String]) extends SPL
case class NAMESPACE(name: String, desc: Option[String], nstype: Option[NamespaceType], isLock: Boolean, ref: Option[String],
                     isXml: Boolean, isJson: Boolean, isSolr: Boolean, maxLines: Option[Long]) extends SPL
case class COLUMN(name: String, aspect: Option[String], ddl: Option[String], attribs: String, as: Option[String],
                  align: Option[String], solrmap: Option[String], kafka: Boolean) extends SPL
case class ICON1() extends SPL // icon_r
case class ICON2() extends SPL // icon_nvpair_r
case class ICON3() extends SPL // icon_nv_unordered_r
case class ICON4() extends SPL // icon_xml_r
case class ICON5() extends SPL // icon_json_r
case class ICON6() extends SPL // icon_align_r
case class ICON7() extends SPL // multi_table_align_r
case class ICON8() extends SPL // icon_apache_r
case class ICON9() extends SPL // icon_csv_r
case class LINEGRAB() extends SPL
case class SETXMLNAMESPACE() extends SPL
case class ADDCONTEXT() extends SPL
case class MULTILINE() extends SPL
case class MULTILINE_BREAK_ON_UNMATCH() extends SPL
case class SKIP() extends SPL
case class OBJECT() extends SPL
case object EXIT extends SPL

trait RegexMatcher[T <: SPL] extends RegexParsers {

  protected def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input): ParseResult[Regex.Match] = {
      if (in.atEnd) {
        Failure("string matching regex `" + r + "' expected but end hit", in.rest)
      } else {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
        println(s"source = $source, offset = $offset, start = $start\n\n")
        r findPrefixMatchOf source.subSequence(start, source.length) match {
          case Some(matched) =>
            println(s"matched = $matched")
            Success(matched,
              in.drop(start + matched.end - offset))
          case None =>
            Failure("string matching regex `" + r + "' expected but `" + in.first + "' found", in.drop(start - offset))
        }
      }
    }
  }

  protected val regex: Regex

  protected def get(values: String*): T

  def apply(): Parser[T] = {
    regexMatch(regex) ^^ { case m => get(m.subgroups: _*) }
  }
}

object NamespaceMatcher extends RegexMatcher[NAMESPACE] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val name = value_list.head
    val desc = Option(value_list(1))
    val nstype = Option(value_list(2)).map(x => NamespaceType.stringToEnum(x))
    val isLock = Option(value_list(3)).isEmpty
    val ref = Option(value_list(4))
    val isXml = Option(value_list(5)).exists(_ == "XML")
    val isJson = Option(value_list(5)).exists(_ == "JSON")
    val isSolr = Option(value_list(6)).isEmpty
    val maxlines = Option(value_list(7)).map(_.toLong)
    NAMESPACE(name, desc, nstype, isLock, ref, isXml, isJson, isSolr, maxlines)
  }

  override protected val regex: Regex = ("""^DEFINE\s+NAMESPACE\s+([\w\._\{\}]+)\s*DESCRIPTION\s+('.+?'|)\s*""" +
    """TYPE\s+(\w+|)\s*(LOCK|)\s*(REF\s+[\w_\.]+|)\s*(XML|JSON|)\s*(SOLR)?\s*(\d+)?""").r
}

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

object ExitMatcher extends RegexMatcher[EXIT.type] {
  override protected def get(values: String*) = EXIT
  override protected val regex: Regex = """\s*;\s*""".r
}

object SplLexer extends RegexParsers {

  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def namespace: Parser[NAMESPACE] = NamespaceMatcher().asInstanceOf[Parser[NAMESPACE]]

  def table: Parser[TABLE] = TableMatcher().asInstanceOf[Parser[TABLE]]

  def exit: Parser[EXIT.type] = ExitMatcher().asInstanceOf[Parser[EXIT.type]]

  def tokens: Parser[List[SPL]] = {
    phrase(rep1(exit | namespace | table))
  }

  def main(args: Array[String]): Unit = {
    val code = Source.fromResource("namespace_table.spl").getLines().mkString("\n")
    println(code)
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        println("error: " + msg)
        Left(SPL_ERROR(msg))
      case Success(result, next) =>
        println("success: " + result)
        Right(result)
    }
  }
}
