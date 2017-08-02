package spl.parser

import spl.lexer.{EXIT, SplLexer, SplToken}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.Parsers

/**
  * Created by bharadwaj on 31/07/17.
  */
object SplPC extends App {
  val tokens: List[SplToken] = (for {
    (code, linenum) <- Source.fromResource("namespace_table.spl").getLines().zipWithIndex
    line = code.trim
    if line.nonEmpty
  } yield {
    SplLexer(line, linenum).right.get
  }).toList

  println(s"tokens = $tokens")

  val ast = SplParser(tokens)
  println(s"ast = $ast")
}

object SplParser extends Parsers {

  type ListOfLists = List[List[SplToken]]

  def apply(tokens: List[SplToken]): SplTopLevel = {
    val splTokenList: ListOfLists = loop(tokens, List())
    buildAST(splTokenList)
  }

  @tailrec
  private def loop(t: List[SplToken], acc: ListOfLists): ListOfLists = {
    val (y: List[SplToken], z: List[SplToken]) = t.span(_ != EXIT)
    println(s"y = $y, z = $z")
    if(z.contains(EXIT)) loop(z.tail, y :: acc)
    else acc
  }

  private def buildAST(splTokenList: ListOfLists): SplTopLevel = {
    null
  }

}