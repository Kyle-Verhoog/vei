package compiler.parser

import compiler.ast.SemanticException
import compiler.scanner.Token
import compiler.parser.Parser.ParseTreeNode

import jlalr.Jlalr1
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Joos1WParser {
  def generateTableLR1(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    Jlalr1.parseLr1(cfg)
  }

  def weedToken(token: Token) {
    token.tokenType match {
      case "IDENTIFIER" =>
        token.value match {
          case "goto" | "double" | "float" | "long" | "switch" | "case" |
          "throws" | "throw" | "do" | "finally" | "break" | "continue" |
          "default" | "synchronized" | "transient" | "volatile" |
          "try" | "catch" =>
            throw SemanticException(
              "illegal value for IDENTIFIER: " + token.value)
            case _ =>
        }
      case "STRING_LITERAL" | "CHARACTER_LITERAL" =>
        if (token.value.contains("\\u")) {
          throw SemanticException(
            "Unicode values are not valid: " + token.value)
        }
      case _ =>
    }
  }

  def parse(tokens: ListBuffer[Token], compilationUnitName: String): ParseTreeNode[Token] = {
    val cfg = Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
    val pt = Parser.parse(cfg, tokens, compilationUnitName, weedToken)
    pt(1)
  }
}
