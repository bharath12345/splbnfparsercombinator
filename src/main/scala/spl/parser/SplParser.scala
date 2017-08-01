package spl.parser

import spl.lexer.{SplLexer, SplToken}

import scala.io.Source
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Created by bharadwaj on 31/07/17.
  */
object SplPC extends App {
  for {
    (code, linenum) <- Source.fromResource("namespace_table.spl").getLines().zipWithIndex
    line = code.trim
    if line.nonEmpty
  } {
    SplLexer(line, linenum)
  }
}

object SplParser extends Parsers {

  override type Elem = SplToken

  class SplTokenReader(tokens: Seq[SplToken]) extends Reader[SplToken] {
    override def first: SplToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[SplToken] = new SplTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[SplToken]): Either[SplParserError, SplAST] = {
    val reader = new SplTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(SplParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}