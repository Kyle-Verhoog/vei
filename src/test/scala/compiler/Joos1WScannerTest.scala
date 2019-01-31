package compiler

import compiler.scanner.{Joos1WScanner, Token}
import org.scalatest.FunSuite

object ScannerTestUtils {
  def assertTokenListsMatch(listA: List[Token], listB: List[Token]): Unit = {
    for (i <- listA.indices) {
      assert(listA(i) == listB(i))
    }
    assert(listA.length == listB.length)
  }
}

class Joos1WScannerTest extends FunSuite {
  test("Single token") {
    Joos1WScanner.generateNewScanner("""IF "if"""")
    val src = "ififif"
    val tokens = Joos1WScanner.scan(src)
    assertTokenListsMatch(
      tokens.toList,
      List(
        new Token("IF", "if"),
        new Token("IF", "if"),
        new Token("IF", "if"),
      )
    )
  }

  test("Single tokens") {
    Joos1WScanner.generateNewScanner("""
      IF "if"
      LPAREN "\("
      RPAREN "\)"
      INT "(1|2)(1|2)*"
      ID "(a|b)(a|b)*"
      SEP ";"
    """)

    val src = "1221;aa;ab;22;()())(;if;"
    val tokens = Joos1WScanner.scan(src)
    assertTokenListsMatch(
      tokens.toList,
      List(
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
      )
    )
  }

  test("Whitespace tokens") {
    Joos1WScanner.generateNewScanner("""
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
    val tokens = Joos1WScanner.scan(src, keepWhitespace = true)
    assertTokenListsMatch(
      tokens.toList,
      List(
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
      )
    )
  }

  test("Whitespace stripped") {
    Joos1WScanner.generateNewScanner("""
      LPAREN "\("
      RPAREN "\)"
      WHITESPACE "☃"
      NEWLINE "☭"
      SEP ";"
    """)

    val src = """ (
    )    ()
    )
    """
    val tokens = Joos1WScanner.scan(src)
    assertTokenListsMatch(
      tokens.toList,
      List(
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("LPAREN", "("),
        new Token("RPAREN", ")"),
        new Token("RPAREN", ")"),
      )
    )
  }

  test("Escaped char literals") {
    // TODO: test char c = '\t'
  }

  test("if statement basic") {
    Joos1WScanner.generateNewScanner(s"""
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
    val tokens = Joos1WScanner.scan(src)
    assertTokenListsMatch(
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

  test("comments") {
    Joos1WScanner.loadSavedScanner()

    val tokens = Joos1WScanner.scan(
      s"""if (ab >= 112 || ab <= 122) {
      ab = baa | baaa; // testing 1 2 3
      //*
      /**/
      /* multiliner

      on multiple lines
      */
      /********/
      /*
       *
       *
       */
      somemorecode = 23;
      // comment 2
      // comment 2
      i = 7/**/*/**/7; // tricky
      /*l*//*d*/ //!
    }""",
      keepComments = true
    )
    assertTokenListsMatch(
      tokens.toList,
      List(
        new Token("IF", "if"),
        new Token("(", "("),
        new Token("IDENTIFIER", "ab"),
        new Token(">=", ">="),
        new Token("INTEGER_LITERAL", "112"),
        new Token("||", "||"),
        new Token("IDENTIFIER", "ab"),
        new Token("<=", "<="),
        new Token("INTEGER_LITERAL", "122"),
        new Token(")", ")"),
        new Token("{", "{"),
        new Token("IDENTIFIER", "ab"),
        new Token("=", "="),
        new Token("IDENTIFIER", "baa"),
        new Token("|", "|"),
        new Token("IDENTIFIER", "baaa"),
        new Token(";", ";"),
        new Token("LINE_COMMENT", "// testing 1 2 3\n"),
        new Token("LINE_COMMENT", "// *\n"),
        new Token("MULTILINE_COMMENT", "‹/**/›"),
        new Token("MULTILINE_COMMENT",
                  "‹/* multiliner\n\n      on multiple lines\n      */›"),
        new Token("MULTILINE_COMMENT", "‹/********/›"),
        new Token("MULTILINE_COMMENT", "‹/*\n       *\n       *\n       */›"),
        new Token("IDENTIFIER", "somemorecode"),
        new Token("=", "="),
        new Token("INTEGER_LITERAL", "23"),
        new Token(";", ";"),
        new Token("LINE_COMMENT", "// comment 2\n"),
        new Token("LINE_COMMENT", "// comment 2\n"),
        new Token("IDENTIFIER", "i"),
        new Token("=", "="),
        new Token("INTEGER_LITERAL", "7"),
        new Token("MULTILINE_COMMENT", "‹/**/›"),
        new Token("*", "*"),
        new Token("MULTILINE_COMMENT", "‹/**/›"),
        new Token("INTEGER_LITERAL", "7"),
        new Token(";", ";"),
        new Token("LINE_COMMENT", "// tricky\n"),
        new Token("MULTILINE_COMMENT", "‹/*l*/›"),
        new Token("MULTILINE_COMMENT", "‹/*d*/›"),
        new Token("LINE_COMMENT", "//!\n"),
        new Token("}", "}"),
      )
    )
  }

  test("if statement and OR, bitwise-OR") {
    Joos1WScanner.generateNewScanner(s"""
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
      WHITESPACE "☃"
      NEWLINE "☭"
    """)
    val tokens = Joos1WScanner.scan(s"""if (ab >= 112 || ab <= 122) {
      ab = baa | baaa;
      ab = baaaaa;
    }""")
    assertTokenListsMatch(
      tokens.toList,
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
    Joos1WScanner.generateNewScanner(s"""
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
      WHITESPACE "☃"
      NEWLINE "☭"
    """)
    val tokens = Joos1WScanner.scan(s"""if (ab >= 112 && ab <= 1222) {
      ab = baa & baaa;
      ab = baaaaa;
    }""")

    assertTokenListsMatch(
      tokens.toList,
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

  test("J1_hello_comment") {
    Joos1WScanner.loadSavedScanner()

    val tokens = Joos1WScanner.scan(
      s"""
// PARSER_WEEDER
public/**/class/*H*/J1_hello_comment/*e*/{/*ll*/
public/*o*/
J1_hello_comment/*,*/
(/* */
)/*w*/
{/*o*/
}/*r*/
/*l*//*d*/ //!
// :-)
/*H*/public/*o*/static/*w*/int/* */test/*a*/(/*r*/)/*e*/{//
String/*y*/s/*o*/=/*u*/"Hello, World!"/*?*/;
/* */System/*I*/./* */out/*a*/./*m*/println/* */(/*f*/s/*i*/)/*n*/;/*e*/
int/*,*/r/* */=/*t*/0/*h*/;/*a*/
for/*n*/(/*k*/int/* */i/*y*/=/*o*/0/*u*/;/*.*/i/* */</*A*/s/*n*/./*d*/length/* */(/*h*/)/*o*/;/*w*/ /* */i/*i*/=/*s*/i/* */+/*y*/1/*o*/)/*u*/ {//r
/*   */r/*a*/=/*u*/17/*n*/*/*t*/r/*?*/+/* */s/*J*/./*o*/charAt/*l*/(/*l*/i/*y*/)/* */;/* g*/
/*o*/}/*o*/
/*d*/return/*.*/248113298/*.*/+/*.*/r/* */;// See
/*y*/}/*o*/
/*u*/}// then, goodbye.
}""",
    )
    println(tokens)
    assert(false)
  }

  def assertTokenListsMatch(listA: List[Token], listB: List[Token]): Unit = {
    for (i <- listA.indices) {
      assert(listA(i) == listB(i))
    }
    assert(listA.length == listB.length)
  }
}
