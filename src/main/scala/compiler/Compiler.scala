package compiler

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import compiler.scanner.Scanner
import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  type State = String

  def main(args: Array[String]) {
    //generateTable()
    scanAndSerialize()

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
    val testProg = Source.fromResource("testfiles/Empty.java").mkString

    val scan = Scanner.fromConfig(tokenDefn)
    println("generated tokens :\n" + scan.scan(testProg))
    Scanner.serializeDfa(scan.dfa, "dfa.txt")
  }

  def scanWithoutSerializing(): Unit = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val testProg = Source.fromResource("testfiles/Empty.java").mkString

    val scan = new Scanner()
    println("generated tokens :\n" + scan.scan(testProg))
  }
}
