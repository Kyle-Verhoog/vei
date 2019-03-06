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
    assert(ClassName(PackageName(""), "A.C") != PackageName("A.C"))
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

  test("ClassName equality") {
    assert(
      ClassName(PackageName(""), "A") ==
        ClassName(PackageName(""), "A"))
    assert(
      ClassName(PackageName("A"), "A") ==
        ClassName(PackageName("A"), "A"))
    assert(
      ClassName(PackageName("A"), "A") !=
        InterfaceName(PackageName("A"), "A"))
    assert(
      ClassName(PackageName("A"), "A") !=
        PackageName("A.A"))
  }

  test("InterfaceName equality") {
    assert(
      InterfaceName(PackageName(""), "A") ==
        InterfaceName(PackageName(""), "A"))
    assert(
      InterfaceName(PackageName("A"), "A") ==
        InterfaceName(PackageName("A"), "A"))
    assert(
      ClassName(PackageName("A"), "A") !=
        InterfaceName(PackageName("A"), "A"))
  }

  test("Comparing Names") {
    assert(
      PackageName("A.B.C").qualifiedName == PackageName("A.B.C").qualifiedName)
    assert(ClassName(PackageName("A.B"), "C").qualifiedName == PackageName(
      "A.B.C").qualifiedName)
    assert(ClassName(PackageName("A.B"), "C") != PackageName("A.B.C"))
    assert(
      ClassName(PackageName("A.B"), "C") != InterfaceName(PackageName("A.B"),
                                                          "C"))
    assert(
      ClassName(PackageName("A.B"), "C") == ClassName(PackageName("A.B"), "C"))
    assert(PackageName("A.B") == PackageName("A.B"))
    assert(ClassName(PackageName(""), "B") == ClassName(PackageName(""), "B"))
  }

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
    // val intClsMap = Map(Pack)
  }
}

class EnvTest extends FunSuite {
  val ABCA_CLS = TestUtils.ASTForSrc(s"""
                                    |package A.B.C;
                                    |public class A {}
       """.stripMargin)
  val ABCA_INT = TestUtils.ASTForSrc(s"""
                                        |package A.B.C;
                                        |interface A {}
       """.stripMargin)
  val BC_CLS = TestUtils.ASTForSrc(s"""
                                          |package B;
                                          |public class C {}
       """.stripMargin)
  val BD_CLS = TestUtils.ASTForSrc(s"""
                                      |package B;
                                      |public class D {}
       """.stripMargin)
  val ABCB_CLS = TestUtils.ASTForSrc(s"""
                                          |package A.B.C;
                                          |public class B {}
       """.stripMargin)
  val A_CLS = TestUtils.ASTForSrc(s"""
                                       |public class A {}
       """.stripMargin)
  val A_INT = TestUtils.ASTForSrc(s"""
                                     |interface A {}
       """.stripMargin)
  val B_CLS = TestUtils.ASTForSrc(s"""
                                       |public class B {}
       """.stripMargin)
  val B_INT = TestUtils.ASTForSrc(s"""
                                     |interface B {}
       """.stripMargin)
  val C_CLS = TestUtils.ASTForSrc(s"""
                                     |public class C {}
       """.stripMargin)
  val D_INT = TestUtils.ASTForSrc(s"""
                                     |interface D {}
       """.stripMargin)

  test("""
      | Global env
    """.stripMargin) {
    val root = new Root().populateNamespace(List(ABCA_CLS, BC_CLS))
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
  }

  test("Root package") {
    var root = new Root().populateNamespace(List(A_CLS))
    assert(root.hasItem(new ClassName(PackageName.ROOT, "A")))
    root = new Root().populateNamespace(List(A_CLS, B_CLS, C_CLS, D_INT))
    assert(root.hasItem(PackageName("")))
    assert(root.hasItem(new ClassName(PackageName.ROOT, "A")))
    assert(root.hasItem(new ClassName(PackageName.ROOT, "B")))
    assert(root.hasItem(new ClassName(PackageName.ROOT, "C")))
    assert(root.hasItem(new InterfaceName(PackageName.ROOT, "D")))
    println(root.toStrTree)
  }

  test("Global class collisions") {
    assertThrows[QualifiedNameCollision](
      new Root().populateNamespace(List(BC_CLS, BC_CLS)))
    val root = new Root().populateNamespace(List(BC_CLS, BD_CLS))
    assert(root.hasItem(ClassName(PackageName("B"), "C")))
    assert(root.hasItem(ClassName(PackageName("B"), "D")))
  }

  test("Global interface collisions") {
    assertThrows[QualifiedNameCollision](
      new Root().populateNamespace(List(ABCA_CLS, ABCA_INT)))
    assertThrows[QualifiedNameCollision](
      new Root().populateNamespace(List(ABCA_INT, ABCA_INT)))
  }

  test("Root package collisions") {
    assertThrows[QualifiedNameCollision](
      new Root().populateNamespace(List(A_CLS, A_CLS)))
    assertThrows[QualifiedNameCollision](
      new Root().populateNamespace(List(A_CLS, A_INT)))
  }

  test("""
      | Resolving imports
    """.stripMargin) {
    /*
    val ast = TestUtils.ASTForSrc(s"""
         |package B;
         |public class C {
         |  public static int main() {
         |    A.B.C.D.E abcde;
         |  }
         |}
       """.stripMargin)
     */
    val root = new Root().populateNamespace(List(ABCA_CLS, ABCB_CLS))
    val qualifiedCls =
      root
        .getItem(ClassName(PackageName("A.B.C"), "A"))
        .asInstanceOf[Some[Class]]
        .get
    assert(qualifiedCls.lookup(Name("A.B.C.A")).isDefined)
    val cls =
      root
        .getItem(Name("A"))
        .asInstanceOf[Some[Class]]
        .get
    assert(cls.lookup(Name("A")).isDefined)
    // assert(cls.globalLookup(Name("")))
  }
}
