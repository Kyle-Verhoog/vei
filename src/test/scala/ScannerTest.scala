import compiler.scanner.Scanner
import org.scalatest.FunSuite

class ScannerTest extends FunSuite {
  test("if statement basic") {
    val scanner = Scanner.fromConfig(s"""IF "if"
      INT "(1|2)(1|2)*"
      VAR "(a|b)(a|b)*"
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
  }

  /*
  test("if statement more basic") {
    val scanner = Scanner.fromConfig(s"""IF "if"
      INT "(1|2)(1|2)*"
      VAR "(a|b)(a|b)*"
      LESSEREQ "<="
      GREATEREQ ">="
      ASSIGN "="
      AND "&&"
      OR "||"
      BITOR "|"
      SEMI ";"
      LPAREN "\\("
      RPAREN "\\)"
      LBRACE "{"
      RBRACE "}"""")
    scanner.scan(s"""if (ab >= 112 && ab <= 123) {
      ab = baa | baaa;
      ab = baaaaa;
    }""")
  }
 */
}
