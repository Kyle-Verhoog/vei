import compiler.scanner.Scanner
import org.scalatest.FunSuite


class ScannerTest extends FunSuite {
  test("Read config") {
    val scanner = Scanner.fromConfig(s"""IF "if"
      INT "(1|2)(1|2)*"
      VAR "(a|b|c|d)"
      ASSIGN "="
      EQ "=="
      SEMI ";"
      LPAREN "["
      RPAREN "]"
      LBRACE "{"
      RBRACE "}"""")
    scanner.scan(s"""if [a == 112] {
      a = b;
    }""")
  }
}
