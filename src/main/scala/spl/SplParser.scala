package spl

import scala.io.Source

/**
  * Created by bharadwaj on 31/07/17.
  */
object SplParser extends App {
  for {
    (code, linenum) <- Source.fromResource("namespace_table.spl").getLines().zipWithIndex
    line = code.trim
    if line.nonEmpty
  } {
    SplLexer(line, linenum)
  }
}
