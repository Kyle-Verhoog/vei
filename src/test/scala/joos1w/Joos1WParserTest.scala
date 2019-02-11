package joos1w

import compiler.ast.SemanticException
import compiler.parser.Joos1WParser
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.{Joos1WScanner, Token}
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.io.Source


class Joos1WParserTest extends FunSuite {
  test("Invalid identifiers") {
    assertThrows[SemanticException](
      Joos1WParser.parse(TestUtils.scanMethod("int switch = 10;"), "A.java")
    )

    assertThrows[SemanticException](
      Joos1WParser.parse(TestUtils.scanMethod("int throws = 10;"), "A.java")
    )

    assertThrows[SemanticException](
      Joos1WParser.parse(TestUtils.scanMethod("int float = 10;"), "A.java")
    )

    assertThrows[SemanticException](
      Joos1WParser.parse(TestUtils.scanClass("""
        private int catch() {
        }
        """), "A.java")
    )
  }
}
