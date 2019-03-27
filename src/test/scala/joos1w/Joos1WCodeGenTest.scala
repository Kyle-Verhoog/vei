package joos1w

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ASMFile
import compiler.joos1w.asm._
import org.scalatest.FunSuite

class Joos1WCodeGenTest extends FunSuite {
  test("ASM formatting") {
    assert(ASM(s"""
          mov eax, eab
      """).code == "mov eax, eab\n")

    assert(ASM(s"""
         | mov eax, eab
       """.stripMargin).code == "mov eax, eab\n")
    assert(ASM(s"""
                  | mov eax, eab
                  | ret
                  |     nop
       """.stripMargin).code == "mov eax, eab\nret\nnop\n")
  }

  test("ASM normal append") {
    assert((ASM(s"""
                  | mov eax, eab
                  |   nop
       """.stripMargin) ++ ASM(s"""
            | nop
            | nop
            | nop
          """.stripMargin)).code == "mov eax, eab\nnop\nnop\nnop\nnop\n")
  }

  test("ASM indent append") {
    assert((ASM(s"""
                   | mov eax, eab
                   |   ret
       """.stripMargin) +++ ASM(s"""
                                  | nop
                                  | nop
                                  | nop
          """.stripMargin)).code == "mov eax, eab\nret\n  nop\n  nop\n  nop\n")
  }

  /*
  test("Single file") {
    val ast = TestUtils.ASTForSrc(s"""
         |public class Z {
         |  public static int test() { return 1; }
         |}
         |""".stripMargin,
                                  "Z.java")
    val code = Joos1WCodeGen.genCode(List(ast))
    assert(code.nonEmpty)
    val astCode = code.head
    assert(astCode.fileName == "__main.s")
    assert(
      astCode.src.trim ==
        """global _start
        |_start:
        |
        |mov ebx, eax
        |mov eax, 1
        |int 0x80
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
    assert(astCode.fileName == "__main.s")
    assert(
      astCode.src.trim ==
        """global _start
          |_start:
          |
          |mov ebx, eax
          |mov eax, 1
          |int 0x80
          |""".stripMargin.trim)
    astCode = code.tail.head
    assert(astCode.fileName == "A_B_C.s")
    assert(astCode.src.trim == """""".stripMargin.trim)
    astCode = code.tail.tail.head
    assert(astCode.fileName == "A_B_D.s")
    assert(astCode.src.trim == """""".stripMargin.trim)
  }
 */
}
