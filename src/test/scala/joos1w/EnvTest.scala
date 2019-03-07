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
    assert(new QualifiedName("A.B") == new PackageName("A.B"))
    assert(new QualifiedName("A.B.C") == ClassName(PackageName("A.B"), "C"))

    assert(
      Map(ClassName(PackageName("A"), "B") -> 0, PackageName("A") -> 1)
        contains
          new QualifiedName("A.B")
    )
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

  test("QualifiedName.toClassName") {
    assert(
      new QualifiedName("A.B.C").toClassName ==
        ClassName(PackageName("A.B"), "C"))
    assert(
      new QualifiedName("A").toClassName ==
        ClassName(PackageName.ROOT, "A"))
  }

  test("QualifiedName.toInterfaceName") {
    assert(
      new QualifiedName("A.B.C").toInterfaceName ==
        InterfaceName(PackageName("A.B"), "C"))
    assert(
      new QualifiedName("A").toInterfaceName ==
        InterfaceName(PackageName.ROOT, "A"))
  }

  test("QualifiedName.toPackageName") {
    assert(
      new QualifiedName("A.B.C").toPackageName ==
        PackageName("A.B.C"))
    assert(
      new QualifiedName("A").toPackageName ==
        PackageName("A"))
    assert(
      new QualifiedName("A.B").toPackageName ==
        PackageName("A.B"))
  }

  test("PackageItemName *") {
    // val t = new QualifiedName("B.D.*").toPackageItemName
    assert(
      new QualifiedName("B.D.*").toPackageItemName.packageName ==
        PackageName("B.D"))
    assert(new QualifiedName("B.D.*").toPackageItemName.itemName == "*")
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

    var mm = Map(new PackageName("") -> 3)
    assert(!(mm contains PackageName("java.lang")))
  }
}

/**
  * Test package and root environments, global namespace
  */
class RootEnvTest extends FunSuite {
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
      | Resolving class/interface name usage within package
    """.stripMargin) {
    val root = new Root().populateNamespace(List(ABCB_CLS, ABCA_INT))
    assert(root.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    assert(root.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    val pkgABC =
      root
        .getItem(PackageName("A.B.C"))
        .asInstanceOf[Some[Package]]
        .get
    assert(pkgABC.lookup(InterfaceName(PackageName("A.B.C"), "A")).isDefined)
    assert(pkgABC.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    assert(pkgABC.lookup(Name("A")).isDefined)
    assert(pkgABC.lookup(Name("B")).isDefined)
    assert(pkgABC.lookup(Name("C")).isEmpty)
  }

  test("""
         | Resolving empty package class/interface name usage within package
       """.stripMargin) {
    val root = new Root().populateNamespace(List(ABCB_CLS, ABCA_INT, C_CLS))
    assert(root.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    assert(root.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    val pkgABC =
      root
        .getItem(PackageName("A.B.C"))
        .asInstanceOf[Some[Package]]
        .get
    assert(pkgABC.lookup(InterfaceName(PackageName("A.B.C"), "A")).isDefined)
    assert(pkgABC.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    assert(pkgABC.lookup(Name("A")).isDefined)
    assert(pkgABC.lookup(Name("B")).isDefined)
    assert(pkgABC.lookup(Name("C")).isDefined)
  }

  test("""
         | Package scoping
         | - Class defined in a parent package should _not_ be visible
       """.stripMargin) {
    val root = new Root().populateNamespace(List(ABCB_CLS, ABCA_INT, C_CLS))
    assert(root.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    assert(root.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    val pkgABC =
      root
        .getItem(PackageName("A.B.C"))
        .asInstanceOf[Some[Package]]
        .get
    assert(pkgABC.lookup(InterfaceName(PackageName("A.B.C"), "A")).isDefined)
    assert(pkgABC.lookup(ClassName(PackageName("A.B.C"), "B")).isDefined)
    assert(pkgABC.lookup(Name("A")).isDefined)
    assert(pkgABC.lookup(Name("B")).isDefined)
    assert(pkgABC.lookup(Name("C")).isDefined)
  }
}

/**
  * Test methods, fields
  */
class ClassEnvTests extends FunSuite {
  val BH = TestUtils.ASTForSrc(s"""
       |package B;
       |public class H {}
       """.stripMargin)
  val CDE = TestUtils.ASTForSrc(s"""
       |package B.D;
       |public class E {}
       """.stripMargin)
  val CDF = TestUtils.ASTForSrc(s"""
       |package B.D;
       |public class F {}
       """.stripMargin)
  val ABD = TestUtils.ASTForSrc(s"""
       |package A.B;
       |
       |public class D {
       |  public static int x = 10;
       |  public int method1(int arg1, boolean arg2) {
       |    D d = null;
       |    A.B.C abc = new A.B.C();
       |    int x;
       |    int y;
       |    for (int i = 0; i < 10; i = i + 1) {
       |      int z;
       |    }
       |    for (x = 0; x < 10; x = x + 1) {
       |      int z;
       |    }
       |    for (; i < 10; i = i + 1) {
       |      int z;
       |    }
       |    {
       |      {
       |        {
       |          int z;
       |        }
       |        int z;
       |      }
       |      int z;
       |    }
       |    int z;
       |  }
       |}
       """.stripMargin)
  val Z = TestUtils.ASTForSrc(s"""
       |public class Z {}
       |""".stripMargin)
  val ABC = TestUtils.ASTForSrc(s"""
       |package A.B;
       |import B.D.*;
       |import B.H;
       |import Z;
       |
       |public class C {
       |  public static int staticVar = 10;
       |  public int method1(int arg1, boolean arg2) {
       |    staticVar = 10;
       |    A.B.C abc = new A.B.C();  // self reference should be resolved to A.B.C
       |    C abc = new A.B.C();  // self reference should be resolved to A.B.C
       |    D d = null; // from package, should be linked to A.B.D
       |    E e = null; // from import-on-demand should be linked to B.D.E
       |    F f = null; // from import-on-demand should be linked to B.D.F
       |    H h = null; // from import, should be linked to B.H;
       |    Z z = null; // from import, should be linked to Z (empty pkg)
       |    int x;
       |    int y;
       |    for (int i = 0; i < 10; i = i + 1) {
       |      int z;
       |    }
       |    for (x = 0; x < 10; x = x + 1) {
       |      int z;
       |    }
       |    for (; i < 10; i = i + 1) {
       |      int z;
       |    }
       |    {
       |      {
       |        {
       |          int z;
       |        }
       |        int z;
       |      }
       |      int z;
       |    }
       |    int z;
       |  }
       |  public int method2(A.B.C arg1, boolean arg2) {
       |    int x;
       |    int y = 4;
       |    if (x < 10)
       |      x = 5;
       |    else
       |      y = 3;
       |
       |    if (x) {
       |      if (x+x) {
       |        int z = 1;
       |      }
       |      int z = 1;
       |    }
       |    else if (y) {
       |      int z = 2;
       |    }
       |    else {
       |      int z = 3;
       |    }
       |  }
       |}
       """.stripMargin)
  test("All methods") {
    println(ABC.toStrTree)
    val root = new Root().populateNamespace(List(BH, CDE, CDF, ABC, ABD, Z))
    println("\n\n\n\n")
    println(root.toStrTree)
  }
}
