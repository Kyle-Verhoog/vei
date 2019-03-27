package joos1w

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ASMFile
import org.scalatest.FunSuite

class Joos1WCodeGenTest extends FunSuite {
  test("Single file") {
    val ast = TestUtils.ASTForSrc(s"""
         |public class Z {}
         |""".stripMargin,
                                  "Z.java")
    val code = Joos1WCodeGen.genCode(List(ast))
    assert(code.nonEmpty)
    val astCode = code.head
    assert(astCode.fileName == "Z.s")
    assert(
      astCode.src.trim ==
        """
        |global _start
        |_start:
      """.stripMargin.trim)
  }

  test("Multiple files") {
    val ast1 = TestUtils.ASTForSrc(s"""
         |public class C {}
         |""".stripMargin,
                                   "A/B/C.java")
    val ast2 = TestUtils.ASTForSrc(s"""
         |public class D {}
         |""".stripMargin,
                                   "A/B/D.java")
    val code = Joos1WCodeGen.genCode(List(ast1, ast2))
    var astCode = code.head
    assert(astCode.fileName == "A_B_C.s")
    assert(
      astCode.src.trim ==
        """
          |global _start
          |_start:
        """.stripMargin.trim)
    astCode = code.tail.head
    assert(astCode.fileName == "A_B_D.s")
    assert(astCode.src.trim == """""".stripMargin.trim)
  }
}
