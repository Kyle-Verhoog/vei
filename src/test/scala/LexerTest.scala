import compiler.Lexer
import org.scalatest.FunSuite;


class LexerTest extends FunSuite  {
    test("Some lexer test") {
        val lexer = new Lexer()
        assert(lexer.lex().equals(""))
    }
}
