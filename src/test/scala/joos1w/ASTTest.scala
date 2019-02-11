package joos1w

import compiler.ast.{AST, MethodDeclaration}
import org.scalatest.FunSuite

class ASTTest extends FunSuite {
  test("getDescendant") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |  }
                                     |}
       """.stripMargin)
    println(ast)
    assert(ast.getDescendant(0).get == ast)
    assert(ast.getDescendant(1).get == ast.leftChild.get)
    assert(ast.getDescendant(2).get == ast.leftChild.get.leftChild.get)
    // TODO check childNum arg
  }

  test("children") {
    val ast = new AST(
      leftChild = Option(
        new AST(
          rightSibling = Option(
            new AST(
              rightSibling = Option(new AST(
                rightSibling = Option(new AST(
                ))
                ))
            ))
        )))

    println(ast)
    assert(ast.children.length == 4)
  }

  test("Methods") {
    val ast = TestUtils.ASTForSrc(s"""
         |public class A {
         |  public static void test() {
         |  }
         |}
       """.stripMargin)
    val method = ast.getDescendant(3)
    method match {
      case Some(n: MethodDeclaration) =>
        assert(n.returnType == "void")
        assert(n.identifier == "test")
        assert(n.modifiers == List("public", "static"))
      case _ => throw new Exception()
    }
  }
}
