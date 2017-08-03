package spl.lexer

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by bharadwaj on 31/07/17.
  */
trait RegexLexer extends RegexParsers {

  type T <: SplToken

  protected def regexMatch(regex: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input): ParseResult[Regex.Match] = {
      if (in.atEnd) {
        Failure(s"string matching regex `$regex' expected but end hit", in.rest)
      } else {
        regex findPrefixMatchOf in.source match {
          case Some(matched) =>
            Success(matched, in.drop(in.source.length()))
          case None =>
            Failure(s"string matching regex `$regex' expected but `" + in.first + "' found", in.rest)
        }
      }
    }
  }

  protected val regex: Regex

  protected def get(values: String*): T

  def apply(): Parser[T] = regexMatch(regex) ^^ { case m => get(m.subgroups.map(x => if(x == null) x else x.trim): _*) }
}
