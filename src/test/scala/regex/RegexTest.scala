import regex.Regex
import org.scalatest.FunSuite;

class RegexPostFixTest extends FunSuite {
  test("No operators") {
    val r = "123456abcd";
    assert(Regex.toPostfix(r) == "12@3@4@5@6@a@b@c@d@")
  }

  test("Parens") {
    val r = "(123)(456)";
    assert(Regex.toPostfix(r) == "12@3@45@6@@")
  }

  test("Nested parens") {
    val r = "((12))((3(4)))";
    assert(Regex.toPostfix(r) == "12@34@@")
  }

  test("Nested nested parens") {
    val r = "((12))((3(45)6))";
    assert(Regex.toPostfix(r) == "12@345@@6@@")
  }

  test("Zero-or-more operator grouped") {
    val r = "(ab)*";
    assert(Regex.toPostfix(r) == "ab@*")
  }

  test("One-or-more operator grouped") {
    val r = "(ab)+";
    assert(Regex.toPostfix(r) == "ab@+")
  }

  test("Zero-or-one operator grouped") {
    val r = "(ab)?";
    assert(Regex.toPostfix(r) == "ab@?")
  }

  test("Alternate operator") {
    val r = "a|b";
    assert(Regex.toPostfix(r) == "ab|")
  }

  test("Multiple alternate operator") {
    val r = "a|b|c";
    assert(Regex.toPostfix(r) == "abc||")
  }

  test("Paren alternate") {
    val r = "a|(b|c)";
    assert(Regex.toPostfix(r) == "abc||")
  }

  test("Alternate 0 or 1") {
    val r = "(a|b)?";
    assert(Regex.toPostfix(r) == "ab|?")
  }

  test("All operators") {
    val r = "(1)+(2)*(3)?(4|5)";
    assert(Regex.toPostfix(r) == "1+2*@3?@45|@")
  }
}

class RegexPostfixToNFA extends FunSuite {
  test("Concatenation simple") {
    val nfa = Regex.postfixToNFA("ab@")
    val start = nfa.startStates.head
    var s = nfa.transitionTable((start, "a")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "b")).head
    assert(nfa.acceptingStates contains s)
  }

  test("Concatenation same char") {
    val nfa = Regex.postfixToNFA("aa@")
    val start = nfa.startStates.head
    var s = nfa.transitionTable((start, "a")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "a")).head
    assert(nfa.acceptingStates contains s)
  }

  test("Concatenation multi") {
    val nfa = Regex.postfixToNFA("ab@c@")
    val start = nfa.startStates.head
    var s = nfa.transitionTable((start, "a")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "b")).head
    s = nfa.transitionTable((s, "ε")).head
    s = nfa.transitionTable((s, "c")).head
    assert(nfa.acceptingStates contains s)
  }

  test("Concatenation NFA.complete()") {
    val nfa = Regex.postfixToNFA("ab@c@")
    assert(!nfa.next("a").isComplete())
    assert(!nfa.next("a").next("b").isComplete())
    assert(nfa.next("a").next("b").next("c").isComplete())
  }
}

class RegexTests extends FunSuite {
  test("Concatenation 3 atoms") {
    val re = Regex.createEngine("abc")
    assert(!re.matches(""))
    assert(!re.matches("a"))
    assert(!re.matches("ab"))
    assert(!re.matches("b"))
    assert(re.matches("abc"))
  }

  test("Alternation 2 atoms") {
    val re = Regex.createEngine("a|b")
    assert(re.matches("a"))
    assert(re.matches("b"))
    assert(!re.matches("ab"))
    assert(!re.matches("aa"))
    assert(!re.matches("bb"))
  }

  test("Concatenation and alternation") {
    val re = Regex.createEngine("a(asdf|1234)")
    assert(re.matches("aasdf"))
    assert(re.matches("a1234"))
    assert(!re.matches("aasd"))
    assert(!re.matches("aasdf1234"))
  }

  test("Nested concatentation and alternation") {
    val re = Regex.createEngine("(asdf|(1|2))(3|4)")
    assert(re.matches("asdf3"))
    assert(re.matches("asdf4"))
    assert(re.matches("13"))
    assert(re.matches("14"))
    assert(!re.matches("asdf1"))
  }
}
