import org.scalatest.FunSuite
import scala.io.Source

import compiler.scanner.{Scanner, Token}

class Joos1WScannerTest extends FunSuite {
  val tokensFile = Source.fromResource("tokens.lex")
  val scanner = Scanner.fromConfig(tokensFile.mkString)
  val whitespaceTokens = Set("WHITESPACE", "NEWLINE")

  def assertTokenListsMatch(listA: List[Token], listB: List[Token]): Unit = {
    for (i <- listA.indices) {
      assert(listA(i) == listB(i))
    }
    assert(listA.length == listB.length)
  }

  test("Scan basic tokens") {
    val rawTokens =
      scanner.scan(Source.fromResource("test/CorrectTokens").mkString)

    val tokens =
      Token.filterTokensByType(rawTokens.toList, whitespaceTokens)

    val verifyingTokens = List(
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
      new Token("||", "||")
    )

    assertTokenListsMatch(tokens, verifyingTokens)
  }

  test("Scan literals") {
    // TODO THIS IS REALLY JUST A NOTE: Run it with this one if you want to really test it (it takes longer)
    val rawTokens = scanner.scan(
      Source.fromResource("test/CorrectLiterals").mkString
    )

    val tokens = Token.filterTokensByType(rawTokens.toList, whitespaceTokens)

    assertTokenListsMatch(
      tokens,
      List(
        new Token("IDENTIFIER", "ABSTRACT"),
        new Token("IDENTIFIER", "BOOLEAN"),
        new Token("IDENTIFIER", "BYTE"),
        new Token("IDENTIFIER", "CHAR"),
        new Token("IDENTIFIER", "CLASS"),
        new Token("IDENTIFIER", "ELSE"),
        new Token("IDENTIFIER", "EXTENDS"),
        new Token("IDENTIFIER", "FINAL"),
        new Token("IDENTIFIER", "FOR"),
        new Token("IDENTIFIER", "IF"),
        new Token("IDENTIFIER", "IMPLEMENTS"),
        new Token("IDENTIFIER", "IMPORT"),
        new Token("IDENTIFIER", "INSTANCEOF"),
        new Token("IDENTIFIER", "INT"),
        new Token("IDENTIFIER", "INTERFACE"),
        new Token("IDENTIFIER", "NATIVE"),
        new Token("IDENTIFIER", "NEW"),
        new Token("IDENTIFIER", "PACKAGE"),
        new Token("IDENTIFIER", "PRIVATE"),
        new Token("IDENTIFIER", "PROTECTED"),
        new Token("IDENTIFIER", "PUBLIC"),
        new Token("IDENTIFIER", "RETURN"),
        new Token("IDENTIFIER", "SHORT"),
        new Token("IDENTIFIER", "STATIC"),
        new Token("IDENTIFIER", "SUPER"),
        new Token("IDENTIFIER", "THIS"),
        new Token("IDENTIFIER", "VOID"),
        new Token("IDENTIFIER", "WHILE"),
        new Token("IDENTIFIER", "someIdentifier"),
        new Token("IDENTIFIER", "some4OtherIdentifier1"),
        new Token("IDENTIFIER", "$myval"),
        new Token("BOOLEAN_LITERAL", "true"),
        new Token("BOOLEAN_LITERAL", "false"),
        new Token("NULL_LITERAL", "null"),
        new Token("INTEGER_LITERAL", "0"),
        new Token("INTEGER_LITERAL", "23420480"),
      )
    )
  }
}
