package joos1w

import compiler.joos1w.ast.{AST, Empty, MethodDeclaration}
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
    val parseTree = TestUtils.parseSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    if (a) {
                                     |      if (b)
                                     |        x = 1;
                                     |      else if (c)
                                     |        x = 1;
                                     |      else
                                     |        x = 1;
                                     |    }
                                     |    else if (d) {
                                     |    }
                                     |    else {
                                     |    }
                                     |  }
                                     |}
       """.stripMargin)(1)
    // println(parseTree.)
    val ast = AST.fromParseTree(parseTree, new Empty)
    println(ast.toStrTree)
  }

  test("Maximal") {
    val src =
      s"""
         |package test;
         |
         |import test.test;
         |import test.test.*;
         |
         |public class A {
         |  static public int x;
         |  public A() {
         |  }
         |  public static void test(int x, int y, int z) {
         |    int[] y = new int[42];
         |    y[0] = 0;
         |    int z = y.length;
         |    this.x = 12313;
         |    {
         |    int i = 5;
         |    String x = "";
         |    x = test.test.x(1, 2, i);
         |    }
         |    {
         |     {
         |       int y = -123;
         |     }
         |    }
         |    if (x) {
         |    return true;
         |    }
         |    else if (x) return true;
         |    else return false;
         |
         |    if (x >= true) {
         |    }
         |    string s = "";
         |    if (x instanceof Y) {
         |    int i = 4;
         |    i = i + 1;
         |    }
         |    else if (y instanceof X) {
         |    int i = 4;
         |    i = i + 1;
         |    }
         |    else {
         |    int i = 4;
         |    i = i + 1;
         |    }
         |
         |    for (int i = 0; i == 10; i = i + 1) {
         |    }
         |
         |    for (i = 0; i > 10; i = i + 1) {
         |    }
         |
         |    for (; i < 10; i = i + 1)
         |      i = i * 10;
         |
         |    while (true) {
         |    }
         |    return true;
         |
         |    while (true)
         |      i = i + 1;
         |  }
         |}
       """.stripMargin
    val parseTree = TestUtils.parseSrc(src)(1)
    // println(parseTree)
    println(TestUtils.ASTForSrc(src).toStrTree)
    val ast = AST.fromParseTree(parseTree)
    println(ast.toStrTree)
  }

  test("Interfaces") {
    val src =
      s"""
         |package test;
         |
         |import test.test;
         |import test.test.*;
         |
         |public interface I extends test.I {
         |}
       """.stripMargin
    val parseTree = TestUtils.parseSrc(src)(1)
    // println(parseTree)
    println(TestUtils.ASTForSrc(src).toStrTree)
    val ast = AST.fromParseTree(parseTree)
    println(ast.toStrTree)
  }

  test("All grammar rules and terminals") {
    // read in grammar.cfg
  }

  test("Classes") {
    val src =
      s"""
         |package test;
         |
         |import test.test;
         |import test.test.*;
         |
         |public class C extends test.C {
         |}
       """.stripMargin
    val parseTree = TestUtils.parseSrc(src)(1)
    println(parseTree)
    // println(parseTree)
    println(TestUtils.ASTForSrc(src).toStrTree)
    val ast = AST.fromParseTree(parseTree)
    println("-----")
    println(ast.toStrTree)
  }
}
