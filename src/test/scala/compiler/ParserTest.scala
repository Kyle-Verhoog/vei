package compiler

import compiler.parser.Parser
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.io.Source

class ParserTest extends FunSuite {
  val simpleLR1CFG = Parser.readInLr1("""6
BOF
EOF
id
-
(
)
3
S
expr
term
S
5
S BOF expr EOF
expr term
expr expr - term
term id
term ( expr )
11
28
8 EOF reduce 2
9 - reduce 4
7 - shift 1
1 id shift 2
6 ( shift 3
6 term shift 4
10 EOF shift 5
2 - reduce 3
4 ) reduce 1
3 id shift 2
4 EOF reduce 1
2 ) reduce 3
0 BOF shift 6
8 - reduce 2
2 EOF reduce 3
3 expr shift 7
9 ) reduce 4
9 EOF reduce 4
4 - reduce 1
1 term shift 8
3 term shift 4
3 ( shift 3
10 - shift 1
6 id shift 2
8 ) reduce 2
1 ( shift 3
7 ) shift 9
6 expr shift 10
    """.split("\n"))

  test("Test readInLr1") {
    val cfg = simpleLR1CFG
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

  test("Parsing simpleLR1") {
    val cfg = simpleLR1CFG

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

    val parseTree = Parser.parse(cfg, tokens, "NO_COMPILATION_NAME", (t: Token) => {})

    val pt = new ParseTreeNode(
      new Token("1", "1"),
      ListBuffer(
        new ParseTreeNode(
          new Token("2", "2"),
          ListBuffer[ParseTreeNode[Token]](
            new ParseTreeNode(
              new Token("3", "3"),
              ListBuffer[ParseTreeNode[Token]]()
            ),
            new ParseTreeNode(
              new Token("5", "5"),
              ListBuffer[ParseTreeNode[Token]]()
            ),
            new ParseTreeNode(
              new Token("6", "6"),
              ListBuffer[ParseTreeNode[Token]]()
            ),
          )
        ),
        new ParseTreeNode(
          new Token("4", "4"),
          ListBuffer[ParseTreeNode[Token]]()
        ),
      )
    )
    println(pt)
  }
}
