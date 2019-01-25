import scala.collection.mutable
import org.scalatest.FunSuite
import regex.{Regex, RegexParseException}

class RegexPreprocessTestSuite extends FunSuite {
  test("Expansion simple") {
    val act = "[a-d]"
    val exp =
      s"${Regex.LPAREN}a${Regex.ALT}b${Regex.ALT}c${Regex.ALT}d${Regex.RPAREN}"
    assert(Regex.preProcess(act) == exp)
  }
}

class RegexParseTestSuite extends FunSuite {
  test("Empty regex") {
    assertThrows[RegexParseException](Regex.toPostfix(""))
  }

  test("Error: missing closing paren") {
    val regex = "(1"
    assertThrows[RegexParseException](Regex.toPostfix(regex))
  }

  test("Error: missing opening paren") {
    val regex = "1)"
    assertThrows[RegexParseException](Regex.toPostfix(regex))
  }

  test("Error: missing atom between parens") {
    val regex = "()"
    assertThrows[RegexParseException](Regex.toPostfix(regex))
  }

  test("Error: missing alt operand") {
    val regex = "*"
    assertThrows[RegexParseException](Regex.toPostfix(regex))
  }

  test("Escaped parens") {
    val regex = "\\(a\\)"
    val exp = s"(a${Regex.CONCAT})${Regex.CONCAT}"
    assert(Regex.toPostfix(regex) == exp)
  }

  test("Escaped brackets") {
    val regex = "\\[a\\]"
    val exp = s"[a${Regex.CONCAT}]${Regex.CONCAT}"
    assert(Regex.toPostfix(regex) == exp)
  }

  test("No operators") {
    val r = "123456abcd"
    assert(Regex.toPostfix(r) == "12·3·4·5·6·a·b·c·d·")
  }

  test("Parens") {
    val r = "(123)(456)"
    assert(Regex.toPostfix(r) == "12·3·45·6··")
  }

  test("Nested parens") {
    val r = "((12))((3(4)))"
    assert(Regex.toPostfix(r) == "12·34··")
  }

  test("Nested nested parens") {
    val r = "((12))((3(45)6))"
    assert(Regex.toPostfix(r) == "12·345··6··")
  }

  test("Zero-or-more operator grouped") {
    val r = "(ab)⨂"
    assert(Regex.toPostfix(r) == "ab·⨂")
  }

  test("One-or-more operator grouped") {
    val r = "(ab)⨁"
    assert(Regex.toPostfix(r) == "ab·⨁")
  }

  test("Zero-or-one operator grouped") {
    val r = "(ab)⁇"
    assert(Regex.toPostfix(r) == "ab·⁇")
  }

  test("Alternate operator") {
    val r = "a∪b"
    assert(Regex.toPostfix(r) == "ab∪")
  }

  test("Multiple alternate operator") {
    val r = "a∪b∪c"
    assert(Regex.toPostfix(r) == "abc∪∪")
  }

  test("Paren alternate") {
    val r = "a∪(b∪c)"
    assert(Regex.toPostfix(r) == "abc∪∪")
  }

  test("Alternate 0 or 1") {
    val r = "(a∪b)⁇"
    assert(Regex.toPostfix(r) == "ab∪⁇")
  }

  test("Not operator") {
    val r = "a¬"
    assert(Regex.toPostfix(r) == "a¬")
  }

  test("All operators") {
    val r = "(1)+(2)*(3)?(4∪5)"
    assert(Regex.toPostfix(r) == "1⨁2⨂·3⁇·45∪·")
  }

  test("Preprocess ranges") {
    val regex = "([a-d]|[X-Z]|[7-9]|_|$)*"
    assert(Regex.preProcess(regex).equals("⦅⦅a∪b∪c∪d⦆∪⦅X∪Y∪Z⦆∪⦅7∪8∪9⦆∪_∪$⦆⨂"))
  }

  test("Preprocess escape special chars") {
    val regex = "(\\*)* (\\() (\\)) \\++ \\?? \\[[0-0] \\][0-0]"
    assert(Regex.preProcess(regex).equals("⦅*⦆⨂ ⦅(⦆ ⦅)⦆ +⨁ ?⁇ [⦅0⦆ ]⦅0⦆"))
  }

}

class RegexPostfixToNFATestSuite extends FunSuite {
  test("Concatenation simple") {
    val nfa = Regex.postfixToNFA("ab·")
    val start = nfa.startStates.head
    var s = nfa.transitionTable((start, "a")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "b")).head
    assert(nfa.acceptingStates contains s)
  }

  test("Concatenation same char") {
    val nfa = Regex.postfixToNFA("aa·")
    val start = nfa.startStates.head
    var s = nfa.transitionTable((start, "a")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "a")).head
    assert(nfa.acceptingStates contains s)
  }

  test("Concatenation multi") {
    val nfa = Regex.postfixToNFA("ab·c·")
    val start = nfa.startStates.head
    var s = nfa.transitionTable((start, "a")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "b")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "c")).head
    assert(nfa.acceptingStates contains s)
  }

  test("Concatenation NFA.complete()") {
    val nfa = Regex.postfixToNFA("ab·c·")
    assert(!nfa.next("a").isComplete())
    assert(!nfa.next("a").next("b").isComplete())
    assert(nfa.next("a").next("b").next("c").isComplete())
  }
}

class RegexTestSuite extends FunSuite {
  test("Sanity") {
    val re = Regex.createEngine("a")
    assert(re.matches("a"))
  }

  test("Concatenation 3 atoms") {
    val re = Regex.createEngine("abc")
    assert(!re.matches(""))
    assert(!re.matches("a"))
    assert(!re.matches("ab"))
    assert(!re.matches("b"))
    assert(re.matches("abc"))
  }

  test("Alternation 2 atoms") {
    val re = Regex.createEngine("a∪b")
    assert(re.matches("a"))
    assert(re.matches("b"))
    assert(!re.matches("ab"))
    assert(!re.matches("aa"))
    assert(!re.matches("bb"))
  }

  test("Concatenation and alternation") {
    val re = Regex.createEngine("a(asdf∪1234)")
    assert(re.matches("aasdf"))
    assert(re.matches("a1234"))
    assert(!re.matches("aasd"))
    assert(!re.matches("aasdf1234"))
  }

  test("Alternation concatenated same character") {
    val re = Regex.createEngine("1(1*)")
    assert(re.matches("1"))
    assert(re.matches("11"))
    assert(re.matches("11111111111111111"))
    assert(!re.matches(""))
    assert(!re.matches("111112"))
  }

  test("Nested concatenation and alternation") {
    val re = Regex.createEngine("(asdf∪(1∪2))(3∪4)")
    assert(re.matches("asdf3"))
    assert(re.matches("asdf4"))
    assert(re.matches("13"))
    assert(re.matches("14"))
    assert(!re.matches("asdf1"))
  }

  test("Zero-or-more 1 atom") {
    val re = Regex.createEngine("a⨂")
    assert(re.matches(""))
    assert(re.matches("a"))
    assert(re.matches("aa"))
    assert(re.matches("aaa"))
    assert(re.matches("aaaaaaaaaaaaaaaaaaaaaaa"))
    assert(!re.matches("b"))
  }

  test("Zero-or-more with alternation") {
    val re = Regex.createEngine("(a∪b)⨂")
    assert(re.matches(""))
    assert(re.matches("a"))
    assert(re.matches("ab"))
    assert(re.matches("aba"))
    assert(re.matches("abbbbaaabbb"))
    assert(re.matches("aaaaaaaaaaaaaaaaaaaaaaab"))
  }

  test("Whitespace") {
    val re = Regex.createEngine(" ")
    assert(re.matches(" "))
    assert(!re.matches(""))
    assert(!re.matches("  "))
  }

  test("Whitespace repeating") {
    val re = Regex.createEngine("( )(( )⨂)")
    assert(re.matches(" "))
    assert(re.matches("     "))
    assert(re.matches("         "))
    assert(re.matches("  "))
    assert(!re.matches(""))
  }

  test("Number") {
    val re = Regex.createEngine("(1|2)(1|2)*")
    assert(re.matches("1"))
    assert(re.matches("11"))
    assert(re.matches("22222"))
  }

  test("Weird Number") {
    val re = Regex.createEngine("(1|2)(1|2)(1|2)*a")
    assert(re.matches("11a"))
    assert(re.matches("1211a"))
    assert(re.matches("22222a"))
  }

  test("1*a") {
    val re = Regex.createEngine("1*a")
    assert(re.matches("a"))
    assert(re.matches("11a"))
  }

  test("3 a's") {
    val re = Regex.createEngine("\"(!|a|\\\")(!|a|\\\")(!|a|\\\")*\"")
    assert(re.matches("\"aaa\""))
  }

  test("Whitespace basic string regex") {
    val re = Regex.createEngine("\"(!|[a-d]|☭|☃|☘|\\\")*\"")
    assert(re.matches("\"abc d ! \n \t\t   !ab \\\" \""))
  }

  test("Dot") {
    val re = Regex.createEngine(".\\.")
    assert(re.matches("\t."))
  }

  test("String regex") {
    val re = Regex.createEngine("\"(!|[#-~]|\\\"|☃|☘)*\"")
    assert(re.matches("\"some_string\\\"\""))
  }

  test("Bad String regex") {
    val re = Regex.createEngine("\"(!|[#-~]|\\\"|☃|☘)*\"")
    assert(!re.matches("\"ab\"a"))
  }

  test("Invalid not regex") {
    assertThrows[RegexParseException](Regex.createEngine(s"(a|b)${Regex.NOT}"))
    // TODO infinite loop:
    // assertThrows[RegexParseException](Regex.createEngine(s"a${Regex.NOT}"))
  }

  test("Not regex") {
    val re = Regex.createEngine(s"a${Regex.NOT}")
    assert(!re.matches("a"))
    assert(re.matches("b"))
    assert(re.matches("c"))
    assert(re.matches("d"))
    assert(re.matches("\n"))
    assert(re.matches("\t"))
  }

  test("Not regex multi") {
    val re = Regex.createEngine(s"b${Regex.NOT}c*")
    assert(re.matches("accccc"))
    assert(re.matches("a"))
    assert(re.matches("acccc"))
    assert(!re.matches("bccccc"))
    assert(!re.matches("b"))
    assert(re.matches("cccc"))
  }

  test("Not regex concat") {
    var re = Regex.createEngine(s"(ab)${Regex.NOT}")
    assert(re.matches("a"))
    assert(re.matches("b"))
    assert(re.matches("x"))
    assert(!re.matches("ab"))
    re = Regex.createEngine(s"c(ab)${Regex.NOT}")
    assert(re.matches("ca"))
    assert(re.matches("cb"))
    assert(!re.matches("cab"))
  }

  test("Not regex concat with another expression") {
    val re = Regex.createEngine(s"b${Regex.NOT}c*")
    assert(re.matches("accccc"))
    assert(re.matches("a"))
    assert(re.matches("acccc"))
    assert(!re.matches("bccccc"))
    assert(!re.matches("b"))
    assert(re.matches("cccc"))
  }

  test("Large regex") {
    val re = Regex.createEngine("([a-z]|[A-Z]|_|$)([a-z]|[A-Z]|[0-9]|_|$)*")
    assert(re.matches("ABSTRACT"))
  }

  test("Zero-or-one single character") {
    val re = Regex.createEngine("a?")
    assert(re.matches(""))
    assert(re.matches("a"))
    assert(!re.matches("b"))
    assert(!re.matches("abbb"))
  }

  test("Zero-or-one two characters") {
    var re = Regex.createEngine("ba?")
    assert(re.matches("b"))
    assert(re.matches("ba"))
    re = Regex.createEngine("a?b")
    assert(re.matches("b"))
    assert(re.matches("ab"))
  }

  test("Zero-or-one with not character") {
    val re = Regex.createEngine(s"a${Regex.NOT}?")
    assert(re.matches(""))
    assert(re.matches("b"))
    assert(re.matches("c"))
  }

  test("One-or-more with not character") {
    val re = Regex.createEngine(s"a${Regex.NOT}*")
    assert(re.matches(""))
    assert(re.matches("b"))
    assert(re.matches("c"))
  }

  test("Comment regex") {
    val re = Regex.createEngine(s"<\\*(>\\*)${Regex.NOT}*\\*>")
    assert(re.matches("<* comment *>"))
    assert(re.matches("<* \n \t *>"))
    assert(re.matches("<* \n \t *>"))
  }

  test("Special characters regex") {
    val re = Regex.createEngine(s"\\*\\(\\)")
    assert(re.matches("*()"))
  }

  test("negative and non negative numbers") {
    val re =
      Regex.createEngine(s"-?(0|((1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)*))")
    assert(re.matches("123"))
    assert(re.matches("0"))
    assert(re.matches("-142"))
  }

  test("special characters in char") {
    val re = Regex.createEngine(s"'[!-a]*'")
    assert(re.matches("'*'"))
    assert(re.matches("')'"))
    assert(re.matches("'('"))
    assert(re.matches("']'"))
    assert(re.matches("'['"))
    assert(re.matches("'?'"))
    assert(re.matches("'+'"))
  }

  test("character regex") {
    val re = Regex.createEngine(s"'((\')|[!-&]|[\\(-~])*'")
    assert(re.matches("''"))
    assert(re.matches("'a'"))
    assert(re.matches("'Z'"))
    assert(re.matches("'az'"))
    assert(!re.matches("'2' + '4' + \"\" + '2' + '4'"))
  }

  test("C comment regex") {
    val re = Regex.createEngine(s"/\\*(\\*/)${Regex.NOT}*\\*/")
    assert(re.matches("/* comment */"))
    assert(re.matches("/**/"))
    assert(!re.matches("/**/ */"))
    assert(!re.matches("/**/*/"))
    assert(!re.matches("/* comment */ ljfdsk\nljkfdsadlkjfa"))
    assert(!re.matches("something /* \n \t */ woah"))
    assert(re.matches("/* \n \t */"))
    // assert(re.matches(s""" <*
    //      | comment
    //      | *>
    //    """.stripMargin))
  }
}

class RegexUtils extends FunSuite {
  test("mergeMaps") {
    val m1 = mutable.HashMap("asdf" -> "TEST")
    val m2 = mutable.HashMap("asd" -> "TEST2")
    val merge = Regex.mergeMaps(m1, m2)

    assert(merge contains "asdf")
    assert(merge contains "asd")
  }
}
