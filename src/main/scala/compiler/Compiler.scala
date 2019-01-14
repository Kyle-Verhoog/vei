package compiler

import compiler.scanner.Scanner
import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  type State = String

  def main(args: Array[String]) {
    generateTable()
    scan()

    if (args.length.equals(0)) {
      println("Must supply a file!")
      return
    }
    println("File given: " + args(0))
  }

  def generateTable(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    println(cfg)
    Jlalr1.parse(cfg)
  }

  def scan(): Unit = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val testProg = Source.fromResource("testfiles/Empty.java").mkString

    val scan = Scanner.fromConfig(tokenDefn)
    println("generated tokens :\n" + scan.scan(testProg))
  }
}
