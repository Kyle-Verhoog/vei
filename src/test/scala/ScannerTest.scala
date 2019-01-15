import compiler.scanner.{Scanner, Token}
import org.scalatest.FunSuite
import regex.Regex

import scala.collection.mutable.ListBuffer
import scala.io.Source

class ScannerTest extends FunSuite {
  test("if statement basic") {
    val scanner = Scanner.fromConfig(s"""IF "if"
      INT "(1|2)(1|2)*"
      ID "(a|b)(a|b)*"
      EQ "=="
      ASSIGN "="
      SEMI ";"
      LPAREN "\\("
      RPAREN "\\)"
      LBRACE "{"
      RBRACE "}"""")
    var tokens = scanner.scan(s"""if (ab == 112) {
      ab = baaaaa;
    }""")
    // println(tokens)
  }

  test("if statement and OR, bitwise-OR") {
    val scanner = Scanner.fromConfig(s"""IF "if"
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
      RBRACE "}"""")
    val tokens = scanner.scan(s"""if (ab >= 112 || ab <= 122) {
      ab = baa | baaa;
      ab = baaaaa;
    }""")
    //println(tokens)
  }

  test("preprocess ranges") {
    val regex = "([a-d]|[X-Z]|[7-9]|_|$)*"
    assert(Regex.preProcess(regex).equals("⦅⦅a∪b∪c∪d⦆∪⦅X∪Y∪Z⦆∪⦅7∪8∪9⦆∪_∪$⦆⨂"))
  }

  test("preprocess escape special chars") {
    val regex = "(\\*)* (\\() (\\)) \\++ \\?? \\[[0-0] \\][0-0]"
    assert(Regex.preProcess(regex).equals("⦅*⦆⨂ ⦅(⦆ ⦅)⦆ +⨁ ?⁇ [⦅0⦆ ]⦅0⦆"))
  }

  test("if statement and AND and bitwise-AND") {
    val scanner = Scanner.fromConfig(s"""IF "if"
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
      RBRACE "}"""")
    val tokens = scanner.scan(s"""if (ab >= 112 && ab <= 1222) {
      ab = baa & baaa;
      ab = baaaaa;
    }""")
    println(tokens)
  }

  test("Scan basic tokens") {
    val scanner = new Scanner()
    var tokens = scanner.scan(Source.fromResource("testfiles/CorrectTokens.txt").mkString)
    tokens = tokens.filter(token => !token.tokenType.equals("NEWLINE") && !token.tokenType.equals("WHITESPACE"))

    var verifyingTokens = ListBuffer[Token](
      new Token("ABSTRACT", "abstract"),
      new Token("BOOLEAN", "boolean"),
      new Token("BYTE", "byte"),
      new Token("CHAR", "char"),
      new Token("CLASS", "class"),
      new Token("ELSE", "else"),
      new Token("EXTENDS", "extends"),
      new Token("FINAL", "final"),
      new Token("FOR", "for"),
      new Token("IF", "if"),
      new Token("IMPLEMENTS", "implements"),
      new Token("IMPORT", "import"),
      new Token("INSTANCEOF", "instanceof"),
      new Token("INT", "int"),
      new Token("INTERFACE", "interface"),
      new Token("NATIVE", "native"),
      new Token("NEW", "new"),
      new Token("PACKAGE", "package"),
      new Token("PRIVATE", "private"),
      new Token("PROTECTED", "protected"),
      new Token("PUBLIC", "public"),
      new Token("RETURN", "return"),
      new Token("SHORT", "short"),
      new Token("STATIC", "static"),
      new Token("SUPER", "super"),
      new Token("THIS", "this"),
      new Token("VOID", "void"),
      new Token("WHILE", "while"),
      new Token("~", "~"),
      new Token("!", "!"),
      new Token("%", "%"),
      new Token("&", "&"),
      new Token("&&", "&&"),
      new Token(",", ","),
      new Token("-", "-"),
      new Token(".", "."),
      new Token("/", "/"),
      new Token(":", ":"),
      new Token(";", ";"),
      new Token("<", "<"),
      new Token("=", "="),
      new Token("!=", "!="),
      new Token(">", ">"),
      new Token("==", "=="),
      new Token("?", "?"),
      new Token("[", "["),
      new Token("]", "]"),
      new Token("{", "{"),
      new Token("}", "}"),
      new Token("^", "^"),
      new Token("*", "*"),
      new Token("(", "("),
      new Token(")", ")"),
      new Token("+", "+"),
      new Token("|", "|"),
      new Token("||", "||"))

    assertTokenListsMatch(tokens.toList, verifyingTokens.toList)
  }

  test("Scan crazy literals") {
    val scanner = new Scanner()
    var tokens = scanner.scan(Source.fromResource("testfiles/CorrectLiterals.txt").mkString)
    tokens = tokens.filter(token => !token.tokenType.equals("NEWLINE") && !token.tokenType.equals("WHITESPACE"))
    // TODO actually verify
  }

    def assertTokenListsMatch(lista: List[Token], listb: List[Token]): Unit = {
    assert(lista.size == listb.size)
    for (i <- 0 until lista.size) {
      assert(lista(i).tokenType.equals(listb(i).tokenType))
      assert(lista(i).value.equals(listb(i).value))
    }
  }
}
