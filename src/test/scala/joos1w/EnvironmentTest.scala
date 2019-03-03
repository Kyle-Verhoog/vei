package joos1w

import exceptions.EnvironmentError
import compiler.joos1w.ast.AST
import compiler.joos1w.environment.environment.{
  buildEnvironment,
  verifyEnvironment
}
import compiler.joos1w.environment.RootEnvironment
import org.scalatest.FunSuite

class EnvironmentTest extends FunSuite {
  test("No two fields declared in same class may have the same name") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static int x = 10;
                                     |  public static int x = 3;
                                     |  public static void test() {
                                     |  }
                                     |}
       """.stripMargin)
    val rootEnvironment = new RootEnvironment(new AST(), None)
    assertThrows[EnvironmentError](
      buildEnvironment(ast, Option(rootEnvironment)))
  }

  test("No two local variables with overlapping scope can have same name") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    int x;
                                     |    int x;
                                     |  }
                                     |}
       """.stripMargin)
    val rootEnvironment = new RootEnvironment(new AST(), None)
    buildEnvironment(ast, Option(rootEnvironment))
  }

  test("No two classes or interfaces have the same canonical name") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    int x;
                                     |    int x;
                                     |  }
                                     |}
       """.stripMargin)
    val rootEnvironment = new RootEnvironment(new AST(), None)
    buildEnvironment(ast, Option(rootEnvironment))
  }
}
