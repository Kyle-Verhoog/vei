package joos1w

import compiler.joos1w.ast.{AST, MethodDeclaration}
import org.scalatest.FunSuite

class ASTTest extends FunSuite {
  test("getDescendant") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |  }
                                     |}
       """.stripMargin)
    assert(ast.getDescendant(0).get == ast)
    assert(ast.getDescendant(1).get == ast.leftChild.get)
    assert(ast.getDescendant(2).get == ast.leftChild.get.leftChild.get)
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

  test("If statements") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    int x = 0;
                                     |    int y = 3;
                                     |    string s = "what";
                                     |    if (x + y > 2 || s == "notwhat") {
                                     |      test();
                                     |    }
                                     |  }
                                     |}
       """.stripMargin)
    println(ast.toStrTree)
  }

  test("If-else-if-else statements") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    int x = 0;
                                     |    int y = 3;
                                     |    string s = "what";
                                     |    if (x + y > 2 || s == "notwhat") {
                                     |      test();
                                     |    }
                                     |    else if (x > 34) {
                                     |      test();
                                     |    }
                                     |    else {
                                     |      string kevin = "420";
                                     |    }
                                     |  }
                                     |}
       """.stripMargin)
    println(ast.toStrTree)
  }
  test("If-else-if-else empty statements") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    if (x) {
                                     |    }
                                     |    else if (x) {
                                     |    }
                                     |    else {
                                     |    }
                                     |  }
                                     |}
       """.stripMargin)
    println(ast.toStrTree)
  }
}
