package joos1w

import exceptions.EnvironmentError
import compiler.joos1w.Joos1WEnvironment
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
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast :: Nil))
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
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast :: Nil))

    val ast2 = TestUtils.ASTForSrc(s"""
                                     |public class A {
                                     |  public static void test() {
                                     |    int x;
                                     |    public static void main() {}
                                     |    int x;
                                     |  }
                                     |}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast2 :: Nil))
  }

  test("No two classes or interfaces have the same canonical name") {
    val ast1 = TestUtils.ASTForSrc(s"""
                                     |package A.B;
                                     |public class A {}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
                                     |package A.B;
                                     |public class A {}
       """.stripMargin)
    val ast3 = TestUtils.ASTForSrc(s"""
                                      |package A.B;
                                      |public interface I {}
       """.stripMargin)
    val ast4 = TestUtils.ASTForSrc(s"""
                                      |package A.B;
                                      |public interface I {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast1 :: ast2 :: Nil))
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast3 :: ast4 :: Nil))
  }

  test("No single-type-import declaration clashes with the class or interface declared in the same file") {
    val ast = TestUtils.ASTForSrc(s"""
                                      |import A.B.A;
                                      |public class A {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast :: Nil))
  }

  test("No two single-type-import declarations clash with each other") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |import A.B.A;
                                     |import A.C.A;
                                     |public class A {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast :: Nil))
  }

  test("All type names must resolve to some class or interface declared in some file") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |package A.B;
                                     |public class C {
                                     |  public C() {
                                     |    A.B.D abc = A.B.D();
                                     |  }
                                     |}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast :: Nil))
  }

  test("All simple type names must resolve to a unique class or interface") {
    val ast = TestUtils.ASTForSrc(s"""
                                     |import A.B.*;
                                     |public class A {
                                     |  public A() {
                                     |    A.B.D notunique = new A.B.D();
                                     |  }
                                     |}
       """.stripMargin)
    val ast1 = TestUtils.ASTForSrc(s"""
                                     |package A.B.C;
                                     |public class D {
                                     |}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
                                     |package A.B.C;
                                     |public class E {}
       """.stripMargin)
    val ast3 = TestUtils.ASTForSrc(s"""
                                     |package A.B.D;
                                     |public class D {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast :: ast1 :: ast2 :: ast3 :: Nil))
  }

  test("""
      When a fully qualified name resolves to a type, no strict prefix of the fully qualified name can resolve to a type
      in the same environment."
    """) {
    val ast1 = TestUtils.ASTForSrc(s"""
                                      |package A.B.C;
                                      |public class A {
                                      |  public static int main() {
                                      |    A.B.D.C abdc = new A.B.D.C();
                                      |  }
                                      |}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
                                      |package A.B.D;
                                      |public class E {}
       """.stripMargin)
    val ast3 = TestUtils.ASTForSrc(s"""
                                      |package A.B;
                                      |public class C {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast1 :: ast2 :: ast3 :: Nil))
  }

  test("""
          No package names or prefixes of package names of declared packages, single-type-import declarations or
          import-on-demand declarations that are used may resolve to types, except for types in the default package
    """) {
    val ast2 = TestUtils.ASTForSrc(s"""
                                      |package A.B.D;
                                      |public class E {}
       """.stripMargin)
    val ast3 = TestUtils.ASTForSrc(s"""
                                      |package A;
                                      |public class C {}
       """.stripMargin)
    val ast4 = TestUtils.ASTForSrc(s"""
                                      |package A.B.C;
                                      |public class C {}
       """.stripMargin)

    val lib = List(ast2, ast3, ast4)

    val test1 = TestUtils.ASTForSrc(s"""
                                      |import A.B.C;
                                      |public class test {
                                      |  public static void main() {
                                      |    A a; // not a type
                                      |  }
                                      |}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(List(test1) ++ lib))

    val test2 = TestUtils.ASTForSrc(s"""
                                       |import A.B.C;
                                       |public class test {
                                       |  public static void main() {
                                       |    A.B a; // not a type
                                       |  }
                                       |}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(List(test2) ++ lib))

    val test3 = TestUtils.ASTForSrc(s"""
                                       |import A.*;
                                       |public class test {
                                       |  public static void main() {
                                       |    A.B.C a; // not a type
                                       |  }
                                       |}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(List(test3) ++ lib))

    val test4 = TestUtils.ASTForSrc(s"""
                                       |public class A {}  // is put in default package
       """.stripMargin)
    val test5 = TestUtils.ASTForSrc(s"""
                                       |import A.B.C;
                                       |public class test {
                                       |  public static void main() {
                                       |    A a; // is in the default package
                                       |  }
                                       |}
       """.stripMargin)
    Joos1WEnvironment.buildEnvironment(List(test4, test5) ++ lib)
  }

  test("""
          Every import-on-demand declaration must refer to a package declared in some file listed on the Joos command
          line. That is, the import-on-demand declaration must refer to a package whose name appears as the package
          declaration in some source file, or whose name is a prefix of the name appearing in some package declaration.
    """) {
    val ast0 = TestUtils.ASTForSrc(s"""
                                      |public class B {}
       """.stripMargin)
    val ast1 = TestUtils.ASTForSrc(s"""
                                      |package A.B.C;
                                      |public class E {}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
                                      |import A.*; // legal
                                      |public class test {}
       """.stripMargin)
    val ast3 = TestUtils.ASTForSrc(s"""
                                      |import A.B.C.*; // legal
                                      |public class test {}
       """.stripMargin)
    Joos1WEnvironment.buildEnvironment(ast0 :: ast1 :: ast2 :: ast3 :: Nil)
    val ast4 = TestUtils.ASTForSrc(s"""
                                      |import B.*; // illegal
                                      |public class test {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast4 :: ast1 :: ast0 :: Nil))
  }

  test("No package names or prefixes of package names") {
    val ast1 = TestUtils.ASTForSrc(s"""
                                      |package A.B.C;
                                      |public class A {
                                      |  public static int main() {
                                      |    A.B.D.C abdc = new A.B.D.C();
                                      |  }
                                      |}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
                                      |package A.B.D;
                                      |public class E {}
       """.stripMargin)
    val ast3 = TestUtils.ASTForSrc(s"""
                                      |package A.B;
                                      |public class C {}
       """.stripMargin)
    assertThrows[EnvironmentError](Joos1WEnvironment.buildEnvironment(ast1 :: ast2 :: ast3 :: Nil))
  }

  test("Two classes with different canonical name") {
    val ast1 = TestUtils.ASTForSrc(s"""
                                      |package A.B;
                                      |public class A {}
       """.stripMargin)
    val ast2 = TestUtils.ASTForSrc(s"""
                                      |package A.B;
                                      |public class C {}
       """.stripMargin)
    Joos1WEnvironment.buildEnvironment(ast1 :: ast2 :: Nil)
  }
}
