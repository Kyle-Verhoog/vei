import regex.Regex
import org.scalatest.FunSuite;

class RegexTest extends FunSuite {
  test("Convert to postfix with parens") {
    val r = "(123)(456)";
    assert(Regex.toPost(r) == "12@3@45@6@@")
  }

  test("Convert to postfix with nested parens") {
    val r = "((12))((3(4)))";
    assert(Regex.toPost(r) == "12@34@@")
  }

  test("Convert to postfix with nested nested parens") {
    val r = "((12))((3(45)6))";
    assert(Regex.toPost(r) == "12@345@@6@@")
  }

  test("Zero-or-more grouped") {
    val r = "(ab)*";
    assert(Regex.toPost(r) == "ab@*")
  }

  test("One-or-more grouped") {
    val r = "(ab)+";
    assert(Regex.toPost(r) == "ab@+")
  }

  test("Zero-or-one grouped") {
    val r = "(ab)?";
    assert(Regex.toPost(r) == "ab@?")
  }
}
