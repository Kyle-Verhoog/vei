package compiler

import compiler.scanner.Scanner
import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  def main(args: Array[String]) {
    generateTable()
    //scanWithoutSerializing()

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

  def scanAndSerialize(): Unit = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val testProg = Source.fromResource("test/Empty.java").mkString

    val scan = Scanner.fromConfig(tokenDefn)
    println("generated tokens :\n" + scan.scan(testProg))
    // println("generated tokens :\n" + scan.scan(testProg).takeRight(10))
    Scanner.serializeDfa(scan.dfa, "dfa_serialization")
  }

  def scanWithoutSerializing(): Unit = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val testProg = Source.fromResource("test/Empty.java").mkString

    val scan = new Scanner()
    println("generated tokens :\n" + scan.scan(testProg))
    // println("generated tokens :\n" + scan.scan(testProg).takeRight(100))
  }
}
