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
