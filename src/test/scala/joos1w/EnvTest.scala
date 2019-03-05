package joos1w

import compiler.joos1w.env._
import compiler.joos1w.env.{
  QualifiedNameCollision,
  Package,
  Root,
  QualifiedName
}
import org.scalatest.FunSuite

class NameTest extends FunSuite {

  test("Name equality") {
    assert(Name("hi") != Name("asdf"))
    assert(Name("hi") == Name("hi"))
  }

  test("isQualified") {
    assert(Name("test.1.2.3").qualified)
  }

  test("Qualified equality") {
    assert(new PackageName("A.B") == new PackageName("A.B"))
    assert(PackageName("A") == PackageName("A"))
    assert(PackageName("A") != PackageName("B"))
    assert(PackageName("A.C") != PackageName("B.A"))
  }

  test("Qualified parents") {
    assert(PackageName("A.B.C").parentPackageNames(0) == PackageName("A.B"))
    assert(
      PackageName("A.B.C").parentPackageNames
        .sameElements(List(PackageName("A.B"), PackageName("A"))))
    assert(PackageName("A").parentPackageNames.sameElements(List()))
  }

  test("ClassName") {
    assert(new ClassName(PackageName(""), "A").className == Name("A"))
    assert(new ClassName(PackageName("A"), "A").className == Name("A"))
    assert(
      new ClassName(PackageName("A"), "A") ==
        new ClassName(PackageName("A"), "A"))
    assert(
      new ClassName(PackageName("A.B.C.D"), "E") !=
        new ClassName(PackageName("A.B.C.D"), "F"))
    assert(
      new ClassName(PackageName("A.B.C.D.e"), "test").className == Name("test"))
    assert(new ClassName(PackageName("A"), "B").parentName == PackageName("A"))
    assert(new ClassName(PackageName(""), "B").parentName == PackageName(""))
  }

  test("PackageName") {}

  test("Hash usage") {
    var m = Map(new Name("hi") -> 0)
    assert(m contains Name("hi"))
    assert(!(m contains Name("h")))
    assert(!(m contains ClassName(PackageName(""), "hi")))
    assert(!(m contains PackageName("hi")))
    m = m + (PackageName("hi") -> 1)
    assert(m contains PackageName("hi"))
    assert(m contains Name("hi"))
    assert(m(Name("hi")) == 0)
    assert(m(PackageName("hi")) == 1)
  }
}

class EnvTest extends FunSuite {
  test("""
      | Global env
    """.stripMargin) {
    val ABCA = TestUtils.ASTForSrc(s"""
         |package A.B.C;
         |public class A {}
       """.stripMargin)
    val BC = TestUtils.ASTForSrc(s"""
         |package B;
         |public class C {}
       """.stripMargin)

    println(ABCA.toStrTree)
    println(BC.toStrTree)
    println("\n\n")
    var root = new Root(List(ABCA, BC))
    root.addPackagesFromASTs()
    println(root.getAllClasses)
    assert(root.hasPackage(PackageName("A.B.C")))
    assert(
      root
        .getPackage(PackageName("A.B.C"))
        .get
        .hasClass(ClassName(PackageName("A.B.C"), "A")))
    assert(root.hasPackage(PackageName("A.B")))
    assert(root.hasPackage(PackageName("B")))
    assert(root.hasPackage(PackageName("A")))
    assert(
      root
        .getPackage(PackageName("B"))
        .get
        .hasClass(ClassName(PackageName("B"), "C")))

    assertThrows[QualifiedNameCollision](
      new Root(List(BC, BC)).addPackagesFromASTs())
    val BA = TestUtils.ASTForSrc(s"""
                                   |package B;
                                   |public class D {}
       """.stripMargin)
    root = new Root(List(BC, BA))
    root.addPackagesFromASTs()
    assert(root.hasItem(ClassName(PackageName("B"), "D")))
    assert(root.hasItem(ClassName(PackageName("B"), "D")))

    println("\n\n\n\n\n")
    // Root package
    val A = TestUtils.ASTForSrc(s"""
                                    |public class A {}
       """.stripMargin)
    root = new Root(List(A))
    root.addPackagesFromASTs()
    println(root.)
    assert(root.hasItem(new ClassName(PackageName.ROOT, "A")))
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
