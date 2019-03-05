package joos1w

import compiler.joos1w.env.{Root, Package}
import org.scalatest.FunSuite

class EnvTest extends FunSuite {
  test("Package.splitName") {
    assert(
      Package.splitName("test.1.2.3").sameElements(List("test", "1", "2", "3")))
    assert(Package.splitName("A").sameElements(List("A")))
  }

  test("Package.parentName") {
    assert(Package.parentName("test.1.2.3") == "test.1.2")
    assert(Package.parentName("A") == Root.ROOT_PKG_NAME)
  }

  test("Package.parentNames") {
    assert(Package.parentNames("A.B.C").sameElements(List("A.B", "A")))
    assert(Package.parentNames("A").sameElements(List()))
  }

  test("""
      | Global env
    """.stripMargin) {
    val ast1 = TestUtils.ASTForSrc(s"""
         |package A.B.C;
         |public class A {}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
         |package B;
         |public class C {}
       """.stripMargin)

    println(ast1.toStrTree)
    println(ast2.toStrTree)
    println("\n\n")
    val root = new Root(List(ast1, ast2))
    root.addPackagesFromASTs()
    println(root.packages)
    assert(root.hasPackage("A.B.C"))
    assert(root.getPackage("A.B.C").get.hasClass("A"))
    assert(root.hasPackage("A.B"))
    assert(root.hasPackage("B"))
    assert(root.hasPackage("A"))
    assert(root.getPackage("B").get.hasClass("C"))
  }

  /*
  test("""
      | Resolving imports
    """.stripMargin) {
    val ast1 = TestUtils.ASTForSrc(s"""
         |import A.B.C.D;
         |public class E {
         |}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
         |package B;
         |public class C {
         |  public static int main() {
         |    A.B.C.D.E abcde;
         |  }
         |}
       """.stripMargin)

    println(ast1.toStrTree)
    println(ast2.toStrTree)
    val root = new Root(List(ast1, ast2))
  }
 */
}
