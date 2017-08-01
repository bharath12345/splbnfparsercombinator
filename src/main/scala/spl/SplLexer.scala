package spl

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object ExitMatcher extends RegexMatcher[EXIT.type] {
  override protected def get(values: String*) = EXIT
  override protected val regex: Regex = """\s*;\s*""".r
}

object SplLexer extends RegexParsers {

  val namespace: Parser[NAMESPACE] = NamespaceMatcher().asInstanceOf[Parser[NAMESPACE]]
  val table: Parser[TABLE] = TableMatcher().asInstanceOf[Parser[TABLE]]
  val exit: Parser[EXIT.type] = ExitMatcher().asInstanceOf[Parser[EXIT.type]]

  val beginsWith: Parser[BEGINS_WITH] = BeginsWithMatcher().asInstanceOf[Parser[BEGINS_WITH]]
  val endsWith: Parser[ENDS_WITH] = EndsWithMatcher().asInstanceOf[Parser[ENDS_WITH]]
  val filepattern: Parser[FILEPATTERN] = FilePatternMatcher().asInstanceOf[Parser[FILEPATTERN]]

  val icon1: Parser[ICON1] = Icon1Matcher().asInstanceOf[Parser[ICON1]]
  val icon2: Parser[ICON2] = Icon1Matcher().asInstanceOf[Parser[ICON2]]
  val icon3: Parser[ICON3] = Icon1Matcher().asInstanceOf[Parser[ICON3]]
  val icon4: Parser[ICON4] = Icon1Matcher().asInstanceOf[Parser[ICON4]]
  val icon5: Parser[ICON5] = Icon1Matcher().asInstanceOf[Parser[ICON5]]
  val icon6: Parser[ICON6] = Icon1Matcher().asInstanceOf[Parser[ICON6]]
  val icon8: Parser[ICON8] = Icon1Matcher().asInstanceOf[Parser[ICON8]]
  val icon9: Parser[ICON9] = Icon1Matcher().asInstanceOf[Parser[ICON9]]

  val linegrab: Parser[LINEGRAB] = Linegrab().asInstanceOf[Parser[LINEGRAB]]
  val setXmlNamespace: Parser[SETXMLNAMESPACE] = SetXmlNamespace().asInstanceOf[Parser[SETXMLNAMESPACE]]
  val addContext: Parser[ADDCONTEXT] = AddContext().asInstanceOf[Parser[ADDCONTEXT]]
  val multiline: Parser[MULTILINE] = Multiline().asInstanceOf[Parser[MULTILINE]]
  val multilineBreakOnUnmatch: Parser[MULTILINE_BREAK_ON_UNMATCH] = MultilineBreakOnUnmatch().asInstanceOf[Parser[MULTILINE_BREAK_ON_UNMATCH]]
  val skip: Parser[SKIP] = Skip().asInstanceOf[Parser[SKIP]]

  val objectM: Parser[OBJECT] = ObjectMatcher().asInstanceOf[Parser[OBJECT]]
  val label: Parser[LABEL] = LabelMatcher().asInstanceOf[Parser[LABEL]]
  val key: Parser[KEY] = KeyMatcher().asInstanceOf[Parser[KEY]]
  val parent: Parser[PARENT] = ParentMatcher().asInstanceOf[Parser[PARENT]]

  val tokens: Parser[List[SPL]] = phrase(rep1(exit | namespace | beginsWith | endsWith | filepattern | table | icon1 | icon2 |
    icon3 | icon4 | icon5 | icon6 | icon8 | icon9 | linegrab | setXmlNamespace | addContext | multiline | multilineBreakOnUnmatch |
    skip | objectM | label | key | parent))

  def main(args: Array[String]): Unit = {
    for {
      (code, linenum) <- Source.fromResource("namespace_table.spl").getLines().zipWithIndex
      line = code.trim
      if line.nonEmpty
    } {
      parse(tokens, code) match {
        case NoSuccess(msg, next) =>
          println(Console.RED + s"linenum: #${linenum}, error: $msg, line = $line" + Console.RESET)
          Left(SPL_ERROR(msg))
        case Success(result, next) =>
          println(Console.GREEN + s"linenum: #${linenum}, success: " + result + Console.RESET)
          Right(result)
      }
    }
  }
}
