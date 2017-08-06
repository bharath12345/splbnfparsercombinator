package com.glassbeam.scalar.core.spl.lexer

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

object ExitLexer extends RegexLexer {
  type T = EXIT.type
  override protected def get(values: String*) = EXIT
  override protected val regex: Regex = """\s*;\s*""".r
}

object SplLexer extends RegexParsers {

  implicit class InstanceOf(x: RegexLexer) {
    // this is just a helper to convert the path dependent type in RegexLexer one level up
    // this is not the best solution because it uses 'asInstanceOf'. But honestly, I dont know how to get rid of
    // the path from a path dependent type in Scala
    def instance[T]: Parser[T] = x().asInstanceOf[Parser[T]]
  }

  private val exit: Parser[EXIT.type] = ExitLexer.instance

  private val namespace: Parser[NAMESPACE] = NamespaceLexer.instance
  private val beginsWith: Parser[BEGINS_WITH] = BeginsWithLexer.instance
  private val endsWith: Parser[ENDS_WITH] = EndsWithLexer.instance
  private val filepattern: Parser[FILEPATTERN] = FilePatternLexer.instance
  private val context: Parser[CONTEXT] = ContextLexer.instance
  private val as: Parser[AS] = AsLexer.instance
  private val bundletype: Parser[BUNDLETYPE] = BundleType.instance

  private val icon1: Parser[ICON1] = Icon1Lexer.instance
  private val icon2: Parser[ICON2] = Icon1Lexer.instance
  private val icon3: Parser[ICON3] = Icon1Lexer.instance
  private val icon4: Parser[ICON4] = Icon1Lexer.instance
  private val icon5: Parser[ICON5] = Icon1Lexer.instance
  private val icon6: Parser[ICON6] = Icon1Lexer.instance
  private val icon8: Parser[ICON8] = Icon1Lexer.instance
  private val icon9: Parser[ICON9] = Icon1Lexer.instance

  private val table: Parser[TABLE] = TableLexer.instance
  private val linegrab: Parser[LINEGRAB] = LinegrabLexer.instance
  private val setXmlNamespace: Parser[SETXMLNAMESPACE] = SetXmlNamespaceLexer.instance
  private val addContext: Parser[ADDCONTEXT] = AddContextLexer.instance
  private val multiline: Parser[MULTILINE] = MultilineLexer.instance
  private val multilineBreakOnUnmatch: Parser[MULTILINE_BREAK_ON_UNMATCH] = MultilineBreakOnUnmatchLexer.instance
  private val skip: Parser[SKIP] = SkipLexer.instance
  private val columns: Parser[COLUMN] = ColumnLexer.instance
  private val colop: Parser[COLOP] = ColOpLexer.instance
  private val rowop: Parser[ROWOP] = RowOpLexer.instance
  private val colCaseOpsLexer: Parser[COPCASEOP] = ColCaseOpsLexer.instance

  private val objectM: Parser[OBJECT] = ObjectLexer.instance
  private val label: Parser[LABEL] = LabelLexer.instance
  private val key: Parser[KEY] = KeyLexer.instance
  private val parent: Parser[PARENT] = ParentLexer.instance

  private val specialTokens: Parser[SplSpecialToken] = exit
  private val namespaceTokens: Parser[SplNamespaceToken] = namespace | beginsWith | endsWith | filepattern | context | as | bundletype
  private val iconTokens: Parser[ICON] = icon1 | icon2 | icon3 | icon4 | icon5 | icon6 | icon8 | icon9
  private val columnTokens: Parser[SplTableToken] = columns | colop | rowop | colCaseOpsLexer
  private val tableTokens: Parser[SplTableToken] = table | iconTokens | linegrab | columnTokens | setXmlNamespace | addContext |
    multiline | multilineBreakOnUnmatch | skip
  private val objectTokens: Parser[SplObjectToken] = objectM | label | key | parent

  // ToDo: optimize this way of extracting tokens; based on what was the type of the last token search only for the next type subset like
  // ToDo: after finding table find its type token tokens till exit
  private val tokens: Parser[List[SplToken]] = phrase(rep1(specialTokens | namespaceTokens | iconTokens | tableTokens | objectTokens))

  def apply(line: String, linenum: Int): Either[SPL_ERROR, SplTokenSuperType] = {
    //println(s"line = $line")
    parse(tokens, line) match {
      case NoSuccess(msg, next) =>
        println(Console.RED + s"linenum: #${linenum}, error: $msg, line = $line" + Console.RESET)
        Left(SPL_ERROR(msg, linenum))
      case Success(result, next) =>
        println(Console.GREEN + s"linenum: #${linenum}, success: " + result + Console.RESET)
        Right(SplTokenSuperType(result.head, linenum))
    }
  }
}
