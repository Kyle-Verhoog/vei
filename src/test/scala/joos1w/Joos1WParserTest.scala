package joos1w

import compiler.joos1w.ast.SemanticException
import compiler.joos1w.Joos1WParser
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token
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
        """),
                         "A.java")
    )
  }
}
