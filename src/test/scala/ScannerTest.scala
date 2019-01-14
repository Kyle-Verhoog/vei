import compiler.scanner.Scanner
import org.scalatest.FunSuite
import regex.Regex

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
    println(tokens)
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

  test("all tokens") {
    println("starting")
    val scanner = Scanner.fromConfig(Source.fromResource("tokens.lex").mkString)
    println("done creating scanner" + scanner.dfa)
    val tokens = scanner.scan(Source.fromResource("testfiles/CorrectTokens.txt").mkString)
    println(tokens)
  }
}
