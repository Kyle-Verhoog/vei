package compiler

import compiler.parser.Parser
import compiler.scanner.{Scanner, Token}
import jlalr.Jlalr1

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Compiler {
  def main(args: Array[String]) {
    //generateTableLR1()

    // filter whitespace
    val tokens = scanWithoutSerializing().filter(
      token =>
        !token.tokenType.equals("NEWLINE") && !token.tokenType
          .equals("WHITESPACE")
    )

    // add BOF and EOF for parsing
    tokens.prepend(new Token("BOF", "bof"))
    tokens.append(new Token("EOF", "eof"))

    val cfg =
      Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
    Parser.parse(cfg, tokens)

    if (args.length.equals(0)) {
      println("Must supply a file!")
      return
    }
    println("File given: " + args(0))
  }

  def generateTableLALR1(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    println(cfg)
    Jlalr1.parse(cfg)
  }

  def generateTableLR1(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    println(cfg)
    Jlalr1.parseLr1(cfg)
  }

  def scanAndSerialize(): Unit = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val testProg = Source.fromResource("test/Empty.java").mkString

    val scan = Scanner.fromConfig(tokenDefn)
    println("generated tokens :\n" + scan.scan(testProg))
    // println("generated tokens :\n" + scan.scan(testProg).takeRight(10))
    Scanner.serializeDfa(scan.dfa, "dfa_serialization")
  }

  def scanWithoutSerializing(): ListBuffer[Token] = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val testProg = Source.fromResource("test/Empty.java").mkString

    val scan = new Scanner()
    //println("generated tokens :\n" + scan.scan(testProg).takeRight(10))
    scan.scan(testProg)
  }
}
