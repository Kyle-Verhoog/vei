import compiler.parser.Parser
import compiler.scanner.Token
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.io.Source

class ParserTest extends FunSuite {
  test("Test reading in example.lr1 file") {
    val lines = Source.fromResource("testfiles/example.lr1").getLines().toArray
    val cfg = Parser.readInLr1(lines)

    assert(cfg.terminals.equals(ListBuffer("BOF", "EOF", "id", "-", "(", ")")))
    assert(cfg.nonterminal.equals(ListBuffer("S", "expr", "term")))
    assert(
      cfg.prodRules.equals(
        ListBuffer(
          List("S", "BOF", "expr", "EOF"),
          List("expr", "term"),
          List("expr", "expr", "-", "term"),
          List("term", "id"),
          List("term", "(", "expr", ")")
        )))

    assert(cfg.reduceActions(8)("EOF").equals(2))
    assert(cfg.reduceActions(9)("-").equals(4))
    assert(cfg.shiftActions(7)("-").equals(1))
    assert(cfg.shiftActions(1)("id").equals(2))
    assert(cfg.shiftActions(6)("(").equals(3))
    assert(cfg.shiftActions(6)("term").equals(4))
    assert(cfg.shiftActions(10)("EOF").equals(5))
    assert(cfg.reduceActions(2)("-").equals(3))
    assert(cfg.reduceActions(4)(")").equals(1))
    assert(cfg.shiftActions(3)("id").equals(2))
    assert(cfg.reduceActions(4)("EOF").equals(1))
    assert(cfg.reduceActions(2)(")").equals(3))
    assert(cfg.shiftActions(0)("BOF").equals(6))
    assert(cfg.reduceActions(8)("-").equals(2))
    assert(cfg.reduceActions(2)("EOF").equals(3))
    assert(cfg.shiftActions(3)("expr").equals(7))
    assert(cfg.reduceActions(9)(")").equals(4))
    assert(cfg.reduceActions(9)("EOF").equals(4))
    assert(cfg.reduceActions(4)("-").equals(1))
    assert(cfg.shiftActions(1)("term").equals(8))
    assert(cfg.shiftActions(3)("term").equals(4))
    assert(cfg.shiftActions(3)("(").equals(3))
    assert(cfg.shiftActions(10)("-").equals(1))
    assert(cfg.shiftActions(6)("id").equals(2))
    assert(cfg.reduceActions(8)(")").equals(2))
    assert(cfg.shiftActions(1)("(").equals(3))
    assert(cfg.shiftActions(7)(")").equals(9))
    assert(cfg.shiftActions(6)("expr").equals(10))
  }

  test("Parsing example.lr1") {
    val lines = Source.fromResource("testfiles/example.lr1").getLines().toArray
    val cfg = Parser.readInLr1(lines)

    // BOF id - ( id ) - id EOF     from cs241
    val tokens = ListBuffer(
      new Token("BOF", "bof"),
      new Token("id", "some_id"),
      new Token("-", "some - "),
      new Token("(", "some ("),
      new Token("id", "some_id 2"),
      new Token(")", "some )"),
      new Token("-", "some - 2"),
      new Token("id", "some_id 3"),
      new Token("EOF", "eof")
    )

    println(Parser.parse(cfg, tokens)(1).inorderLeafValues())
  }
}
