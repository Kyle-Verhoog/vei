package joos1w

import compiler.joos1w.env.Root
import org.scalatest.FunSuite

class EnvTest extends FunSuite {
  test(
    """
      | Global env
    """.stripMargin) {
    val ast1 = TestUtils.ASTForSrc(
      s"""
         |package A.B;
         |public class A {}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(
      s"""
         |package B;
         |public class C {}
       """.stripMargin)

    println(ast1.toStrTree)
    println(ast2.toStrTree)
    val root = Root.makeRoot(ast1).addAST(Some(ast2))
    assert(root.hasPackage("A.B"))
    assert(root.getPackage("A.B").hasClass("A"))
    assert(root.hasPackage("B"))
    assert(root.getPackage("B").hasClass("C"))
    assert(root.hasPackage("A"))
  }
}
