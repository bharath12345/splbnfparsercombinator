package spl.parser

import spl.lexer.{EXIT, SplLexer, SplToken}
import spl.parser.TokenSetType.TokenSetType

import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.Parsers

/**
  * Created by bharadwaj on 31/07/17.
  */
object SplPC extends App {
  val tokens: SplTokenList = (for {
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

  def apply(tokens: SplTokenList): SplTopLevel = {
    val listOfTokenSets: ListOfSplTokenSets = loop(tokens)
    validateAndMap(listOfTokenSets)
    buildAST(listOfTokenSets)
  }

  @tailrec
  private def loop(t: SplTokenList, acc: ListOfSplTokenSets = List()): ListOfSplTokenSets = {
    val (y: SplTokenList, z: SplTokenList) = t.span(_ != EXIT)
    println(s"y = $y, z = $z")
    if(z.contains(EXIT)) loop(z.tail, y.toSet :: acc)
    else acc
  }

  private def validateAndMap(listOfTokenSets: ListOfSplTokenSets): Map[TokenSetType, Set[SplToken]] = {
    listOfTokenSets.map { splTokenSet: Set[SplToken] =>
      //splTokenSet.
    }
    null
  }

  private def buildAST(splTokenList: ListOfSplTokenSets): SplTopLevel = {

    null
  }

}