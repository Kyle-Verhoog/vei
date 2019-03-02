package joos1w

import compiler.joos1w.ast.AST
import compiler.joos1w.Joos1WScanner
import compiler.parser.Parser
import compiler.parser.Parser.{CFG, ParseTreeNode}
import compiler.scanner.Token

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

  def getMarmosetLibFiles(version: String): List[String] = {
    getAllFilesInSubDirectories("test/marmoset/lib/" + version + "/java/")
  }

  def getAllFilesInSubDirectories(dir: String): List[String] = {
    val subdirs = Source
      .fromResource(dir)
      .mkString
      .split("\n")
      .filter(s => !s.contains(".java"))
    val subFiles: List[String] =
      subdirs
        .flatMap(newDir => getAllFilesInSubDirectories(dir + "/" + newDir))
        .toList

    val files = Source
      .fromResource(dir)
      .mkString
      .split("\n")
      .filter(s => s.contains(".java"))
      .map(s => dir + "/" + s)
      .distinct
      .toList

    files ++ subFiles
  }

  def marmosetTestFiles(assignmentVersion: String): List[List[String]] = {
    // get all single files
    val singleFiles = Source
      .fromResource("test/marmoset/" + assignmentVersion)
      .mkString
      .split('\n')
      .filter(s => s.contains(".java"))
      .map(s => List("test/marmoset/" + assignmentVersion + "/" + s))
      .distinct
      .toList

    // get directories
    val subDirFiles = Source
      .fromResource("test/marmoset/" + assignmentVersion)
      .mkString
      .split('\n')
      .filter(s => !s.contains(".java"))
      .map(dir => getAllFilesInSubDirectories("test/marmoset/" + assignmentVersion + "/" + dir))
      .toList

    subDirFiles ++ singleFiles
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
    AST.fromParseTree(parseTree)
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
    scan(base(classBody = body))
  }
}
