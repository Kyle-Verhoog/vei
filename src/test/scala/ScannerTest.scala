import scala.collection.mutable.ListBuffer
import org.scalatest.FunSuite
import compiler.scanner.{Scanner, Token}

object ScannerTestUtils {
  def assertTokenListsMatch(listA: List[Token], listB: List[Token]): Unit = {
    for (i <- listA.indices) {
      assert(listA(i) == listB(i))
    }
    assert(listA.length == listB.length)
  }
}

class ScannerTest extends FunSuite {
  test("Single token") {
    val scanner = Scanner.fromConfig("""IF "if"""")
    val src = "ififif"
    val tokens = scanner.scan(src)
    ScannerTestUtils.assertTokenListsMatch(
      tokens.toList,
      ListBuffer[Token](
        new Token("IF", "if"),
        new Token("IF", "if"),
        new Token("IF", "if"),
      ).toList
    )
  }

  test("Single token") {
    val scanner = Scanner.fromConfig("""IF "if"""")
    val src = "ififif"
    val tokens = scanner.scan(src)
    ScannerTestUtils.assertTokenListsMatch(
      tokens.toList,
      ListBuffer[Token](
        new Token("IF", "if"),
        new Token("IF", "if"),
        new Token("IF", "if"),
      ).toList
    )
  }

  test("Single tokens") {
    val scanner = Scanner.fromConfig("""
      IF "if"
      LPAREN "\("
      RPAREN "\)"
      INT "(1|2)(1|2)*"
      ID "(a|b)(a|b)*"
      SEP ";"
    """)

    val src = "1221;aa;ab;22;()())(;if;"
    val tokens = scanner.scan(src)
    ScannerTestUtils.assertTokenListsMatch(
      tokens.toList,
      ListBuffer[Token](
        new Token("INT", "1221"),
        new Token("SEP", ";"),
        new Token("ID", "aa"),
        new Token("SEP", ";"),
        new Token("ID", "ab"),
        new Token("SEP", ";"),
        new Token("INT", "22"),
        new Token("SEP", ";"),
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("RPAREN", ")"),
        new Token("LPAREN", "("),
        new Token("SEP", ";"),
        new Token("IF", "if"),
        new Token("SEP", ";"),
      ).toList
    )
  }

  test("Whitespace tokens") {
    val scanner = Scanner.fromConfig("""
      LPAREN "\("
      RPAREN "\)"
      SPACE "☃"
      NEWLINE "☭"
      SEP ";"
    """)

    val src = """ (
    )    ()
    )
    """
    val tokens = scanner.scan(src)
    ScannerTestUtils.assertTokenListsMatch(
      tokens.toList,
      ListBuffer[Token](
        new Token("SPACE", " "),
        new Token("LPAREN", "("),
        new Token("NEWLINE", "\n"),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("RPAREN", ")"),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("NEWLINE", "\n"),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("RPAREN", ")"),
        new Token("NEWLINE", "\n"),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
        new Token("SPACE", " "),
      ).toList
    )
  }

  test("Whitespace stripped") {
    val scanner = Scanner.fromConfig("""
      LPAREN "\("
      RPAREN "\)"
      SPACE "☃"
      NEWLINE "☭"
      SEP ";"
    """)

    val src = """ (
    )    ()
    )
    """
    val tokens = scanner.scan(src)
    ScannerTestUtils.assertTokenListsMatch(
      Token.filterTokensByType(tokens.toList, Set("NEWLINE", "SPACE")),
      ListBuffer[Token](
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("RPAREN", ")"),
      ).toList
    )
  }

  test("Escaped char literals") {
    // TODO: test char c = '\t'
  }

  test("if statement basic") {
    val scanner = Scanner.fromConfig(s"""
      IF "if"
      INT "(1|2)(1|2)*"
      ID "(a|b)(a|b)*"
      EQ "=="
      ASSIGN "="
      SEMI ";"
      LPAREN "\\("
      RPAREN "\\)"
      LBRACE "{"
      RBRACE "}"
    """)

    val src = "if(ab==112){ab=baaaaa;}"
    val tokens = scanner.scan(src)
    ScannerTestUtils.assertTokenListsMatch(
      tokens.toList,
      List(
        new Token("IF", "if"),
        new Token("LPAREN", "("),
        new Token("ID", "ab"),
        new Token("EQ", "=="),
        new Token("INT", "112"),
        new Token("RPAREN", ")"),
        new Token("LBRACE", "{"),
        new Token("ID", "ab"),
        new Token("ASSIGN", "="),
        new Token("ID", "baaaaa"),
        new Token("SEMI", ";"),
        new Token("RBRACE", "}"),
      )
    )
  }

  test("if statement and OR, bitwise-OR") {
    val scanner = Scanner.fromConfig(s"""
      IF "if"
      INT "(1|2)(1|2)*"
      ID "(a|b)(a|b)*"
      LESSEREQ "<="
      GREATEREQ ">="
      ASSIGN "="
      AND "&&"
      OR "\\|\\|"
      BITOR "\\|"
      SEMI ";"
      LPAREN "\\("
      RPAREN "\\)"
      LBRACE "{"
      RBRACE "}"
      SPACE "☃"
      NEWLINE "☭"
    """)
    val tokens = scanner.scan(s"""if (ab >= 112 || ab <= 122) {
      ab = baa | baaa;
      ab = baaaaa;
    }""")
    ScannerTestUtils.assertTokenListsMatch(
      Token.filterTokensByType(tokens.toList, Set("SPACE", "NEWLINE")),
      List(
        new Token("IF", "if"),
        new Token("LPAREN", "("),
        new Token("ID", "ab"),
        new Token("GREATEREQ", ">="),
        new Token("INT", "112"),
        new Token("OR", "||"),
        new Token("ID", "ab"),
        new Token("LESSEREQ", "<="),
        new Token("INT", "122"),
        new Token("RPAREN", ")"),
        new Token("LBRACE", "{"),
        new Token("ID", "ab"),
        new Token("ASSIGN", "="),
        new Token("ID", "baa"),
        new Token("BITOR", "|"),
        new Token("ID", "baaa"),
        new Token("SEMI", ";"),
        new Token("ID", "ab"),
        new Token("ASSIGN", "="),
        new Token("ID", "baaaaa"),
        new Token("SEMI", ";"),
        new Token("RBRACE", "}"),
      )
    )
  }

  test("if statement and AND and bitwise-AND") {
    val scanner = Scanner.fromConfig(s"""
      IF "if"
      INT "(1|2)(1|2)*"
      ID "(a|b)(a|b)*"
      LESSEREQ "<="
      GREATEREQ ">="
      ASSIGN "="
      AND "&&"
      BITAND "&"
      SEMI ";"
      LPAREN "\\("
      RPAREN "\\)"
      LBRACE "{"
      RBRACE "}"
      SPACE "☃"
      NEWLINE "☭"
    """)
    val tokens = scanner.scan(s"""if (ab >= 112 && ab <= 1222) {
      ab = baa & baaa;
      ab = baaaaa;
    }""")

    assertTokenListsMatch(
      Token.filterTokensByType(tokens.toList, Set("SPACE", "NEWLINE")),
      List(
        new Token("IF", "if"),
        new Token("LPAREN", "("),
        new Token("ID", "ab"),
        new Token("GREATEREQ", ">="),
        new Token("INT", "112"),
        new Token("AND", "&&"),
        new Token("ID", "ab"),
        new Token("LESSEREQ", "<="),
        new Token("INT", "1222"),
        new Token("RPAREN", ")"),
        new Token("LBRACE", "{"),
        new Token("ID", "ab"),
        new Token("ASSIGN", "="),
        new Token("ID", "baa"),
        new Token("BITAND", "&"),
        new Token("ID", "baaa"),
        new Token("SEMI", ";"),
        new Token("ID", "ab"),
        new Token("ASSIGN", "="),
        new Token("ID", "baaaaa"),
        new Token("SEMI", ";"),
        new Token("RBRACE", "}"),
      )
    )
  }

  def assertTokenListsMatch(listA: List[Token], listB: List[Token]): Unit = {
    for (i <- listA.indices) {
      assert(listA(i) == listB(i))
    }
    assert(listA.length == listB.length)
  }
}
