package spl

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object ExitMatcher extends RegexMatcher[EXIT.type] {
  override protected def get(values: String*) = EXIT
  override protected val regex: Regex = """\s*;\s*""".r
}

object SplLexer extends RegexParsers {

  def namespace: Parser[NAMESPACE] = NamespaceMatcher().asInstanceOf[Parser[NAMESPACE]]
  def table: Parser[TABLE] = TableMatcher().asInstanceOf[Parser[TABLE]]
  def exit: Parser[EXIT.type] = ExitMatcher().asInstanceOf[Parser[EXIT.type]]

  def beginsWith: Parser[BEGINS_WITH] = BeginsWithMatcher().asInstanceOf[Parser[BEGINS_WITH]]
  def endsWith: Parser[ENDS_WITH] = EndsWithMatcher().asInstanceOf[Parser[ENDS_WITH]]
  def filepattern: Parser[FILEPATTERN] = FilePatternMatcher().asInstanceOf[Parser[FILEPATTERN]]

  def tokens: Parser[List[SPL]] = phrase(rep1(exit | namespace | beginsWith | endsWith | filepattern | table))

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
