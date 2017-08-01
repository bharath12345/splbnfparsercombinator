package spl

import scala.util.parsing.input.Positional

/**
  * Created by bharadwaj on 31/07/17.
  */
package object parser {

  sealed trait SplAST extends Positional
  case class NamespaceAST() extends SplAST
  case class TableAST() extends SplAST
  case class ObjectAST() extends SplAST
  case object ExitAST extends SplAST

  sealed trait SplCompilationError
  case class SplLexerError(location: Location, msg: String) extends SplCompilationError
  case class SplParserError(location: Location, msg: String) extends SplCompilationError

  case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
  }
}
