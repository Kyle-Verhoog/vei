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
    Parser.parse(cfg, tokens, "NO_COMPILATION_NAME", (t: Token) => {})
  }

  def genAST(parseTree: ParseTreeNode[Token]): AST = {
    AST.convertParseTree(parseTree)
  }

  def scan(src: String): ListBuffer[Token] = {
    Joos1WScanner.loadSavedScanner()
    Joos1WScanner.scan(src)
  }

  def parseSrc(src: String): ListBuffer[ParseTreeNode[Token]] = {
    parseTokens(scan(src))
  }

  def ASTForSrc(src: String): AST = {
    genAST(parseSrc(src)(1))
  }

  def base(methodBody: String = "", classBody: String = ""): String = {
    s"""
    public class A {
      public static int test() {
        $methodBody
      }
      $classBody
    }
    """
  }

  def scanMethod(body: String = ""): ListBuffer[Token] = {
    scan(base(body))
  }

  def scanClass(body: String = ""): ListBuffer[Token] = {
    scan(base(classBody=body))
  }
}
