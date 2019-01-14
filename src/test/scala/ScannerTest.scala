import compiler.scanner.Scanner
import org.scalatest.FunSuite

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
}
