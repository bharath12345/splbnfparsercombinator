package spl.lexer

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object ExitMatcher extends RegexMatcher[EXIT.type] {
  override protected def get(values: String*) = EXIT
  override protected val regex: Regex = """\s*;\s*""".r
}

object SplLexer extends RegexParsers {

  private val namespace: Parser[NAMESPACE] = NamespaceMatcher().asInstanceOf[Parser[NAMESPACE]]
  private val table: Parser[TABLE] = TableMatcher().asInstanceOf[Parser[TABLE]]
  private val exit: Parser[EXIT.type] = ExitMatcher().asInstanceOf[Parser[EXIT.type]]

  private val beginsWith: Parser[BEGINS_WITH] = BeginsWithMatcher().asInstanceOf[Parser[BEGINS_WITH]]
  private val endsWith: Parser[ENDS_WITH] = EndsWithMatcher().asInstanceOf[Parser[ENDS_WITH]]
  private val filepattern: Parser[FILEPATTERN] = FilePatternMatcher().asInstanceOf[Parser[FILEPATTERN]]

  private val icon1: Parser[ICON1] = Icon1Matcher().asInstanceOf[Parser[ICON1]]
  private val icon2: Parser[ICON2] = Icon1Matcher().asInstanceOf[Parser[ICON2]]
  private val icon3: Parser[ICON3] = Icon1Matcher().asInstanceOf[Parser[ICON3]]
  private val icon4: Parser[ICON4] = Icon1Matcher().asInstanceOf[Parser[ICON4]]
  private val icon5: Parser[ICON5] = Icon1Matcher().asInstanceOf[Parser[ICON5]]
  private val icon6: Parser[ICON6] = Icon1Matcher().asInstanceOf[Parser[ICON6]]
  private val icon8: Parser[ICON8] = Icon1Matcher().asInstanceOf[Parser[ICON8]]
  private val icon9: Parser[ICON9] = Icon1Matcher().asInstanceOf[Parser[ICON9]]

  private val linegrab: Parser[LINEGRAB] = Linegrab().asInstanceOf[Parser[LINEGRAB]]
  private val setXmlNamespace: Parser[SETXMLNAMESPACE] = SetXmlNamespace().asInstanceOf[Parser[SETXMLNAMESPACE]]
  private val addContext: Parser[ADDCONTEXT] = AddContext().asInstanceOf[Parser[ADDCONTEXT]]
  private val multiline: Parser[MULTILINE] = Multiline().asInstanceOf[Parser[MULTILINE]]
  private val multilineBreakOnUnmatch: Parser[MULTILINE_BREAK_ON_UNMATCH] = MultilineBreakOnUnmatch().asInstanceOf[Parser[MULTILINE_BREAK_ON_UNMATCH]]
  private val skip: Parser[SKIP] = Skip().asInstanceOf[Parser[SKIP]]

  private val objectM: Parser[OBJECT] = ObjectMatcher().asInstanceOf[Parser[OBJECT]]
  private val label: Parser[LABEL] = LabelMatcher().asInstanceOf[Parser[LABEL]]
  private val key: Parser[KEY] = KeyMatcher().asInstanceOf[Parser[KEY]]
  private val parent: Parser[PARENT] = ParentMatcher().asInstanceOf[Parser[PARENT]]

  private val tokens: Parser[List[SplToken]] = phrase(rep1(exit | namespace | beginsWith | endsWith | filepattern | table | icon1 | icon2 |
    icon3 | icon4 | icon5 | icon6 | icon8 | icon9 | linegrab | setXmlNamespace | addContext | multiline | multilineBreakOnUnmatch |
    skip | objectM | label | key | parent))

  def apply(line: String, linenum: Int): Either[SPL_ERROR, List[SplToken]] = {
    //println(s"line = $line")
    parse(tokens, line) match {
      case NoSuccess(msg, next) =>
        println(Console.RED + s"linenum: #${linenum}, error: $msg, line = $line" + Console.RESET)
        Left(SPL_ERROR(msg))
      case Success(result, next) =>
        println(Console.GREEN + s"linenum: #${linenum}, success: " + result + Console.RESET)
        Right(result)
    }
  }
}
