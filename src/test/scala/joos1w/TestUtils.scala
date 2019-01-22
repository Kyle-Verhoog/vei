package joos1w

import compiler.ast.AST
import compiler.parser.Parser
import compiler.parser.Parser.{CFG, ParseTreeNode}
import compiler.scanner.{Joos1WScanner, Scanner, Token}

import scala.collection.mutable.ListBuffer
import scala.io.Source

object TestUtils {
  def featureTestFiles(): List[String] = {
    Source
      .fromResource("test/features/")
      .mkString
      .split('\n')
      .map(s => s.split('.')(0))
      .distinct
      .toList
  }

  def marmosetTestFiles(assignmentVersion: String): List[String] = {
    Source
      .fromResource("test/marmoset/" + assignmentVersion)
      .mkString
      .split('\n')
      .map(s => s.split('.')(0))
      .distinct
      .toList
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
    Joos1WScanner.loadSavedScanner()
    parseTokens(Joos1WScanner.scan(src))
  }

  def ASTForSrc(src: String): AST = {
    genAST(parseSrc(src)(1))
  }
}
