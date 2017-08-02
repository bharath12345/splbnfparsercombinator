package spl.parser

import spl.lexer.{EXIT, SplLexer, SplNamespaceToken, SplObjectToken, SplTableToken, SplToken}
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

  private def validateTokenSet(tokenSet: Set[SplToken]): TokenSetType = {
    def getType(token: SplToken): TokenSetType = {
      token match {
        case ns: SplNamespaceToken => TokenSetType.Namespace
        case tb: SplTableToken => TokenSetType.Table
        case ob: SplObjectToken => TokenSetType.Object
        case _ => throw new Exception(s"Unknown type for token = ${token}")
      }
    }

    val headType: TokenSetType = getType(tokenSet.head)
    tokenSet.tail.foreach { token: SplToken =>
      val tokenType = getType(token)
      if(tokenType != headType)
        throw new Exception(s"Inconsistent token type in set = ${tokenSet}")
    }
    headType
  }

  private def validateAndMap(listOfTokenSets: ListOfSplTokenSets): Map[TokenSetType, Set[SplToken]] = {
    listOfTokenSets.map { splTokenSet: Set[SplToken] =>
      (validateTokenSet(splTokenSet), splTokenSet)
    }.toMap
  }

  private def buildAST(splTokenList: ListOfSplTokenSets): SplTopLevel = {
    null
  }

}