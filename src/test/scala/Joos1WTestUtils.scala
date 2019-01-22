import compiler.ast.AST
import compiler.parser.Parser
import compiler.parser.Parser.{CFG, ParseTreeNode}

import scala.io.Source
import compiler.scanner.{Scanner, Token}

import scala.collection.mutable.ListBuffer

object Joos1WTestUtils {
  def featureTestFiles(): List[String] = {
    Source
      .fromResource("test/features/")
      .mkString
      .split('\n')
      .map(s => s.split('.')(0))
      .distinct
      .toList
  }

  def testScanner(): Scanner = {
    new Scanner()
  }

  /**
    * Scans tokens and strips whitespace
    * @param src source text
    */
  def scanTokens(src: String): ListBuffer[Token] = {
    val tokens = testScanner()
      .scan(src)
      .filter(t => !Set("WHITESPACE", "NEWLINE").contains(t.tokenType))
    tokens.prepend(new Token("BOF", "bof"))
    tokens.append(new Token("EOF", "eof"))
    tokens
  }

  def grammar: CFG = {
    Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
  }

  def parseTokens(
      tokens: ListBuffer[Token]): ListBuffer[ParseTreeNode[Token]] = {
    val cfg = grammar
    Parser.parse(cfg, tokens)
  }

  def genAST(parseTree: ParseTreeNode[Token]): AST = {
    AST.convertParseTree(parseTree)
  }

  def parseSrc(src: String): ListBuffer[ParseTreeNode[Token]] = {
    parseTokens(scanTokens(src))
  }

  def ASTForSrc(src: String): AST = {
    genAST(parseSrc(src)(1))
  }
}
