package compiler

import compiler.scanner.Scanner
import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  type State = String

  def main(args: Array[String]) {
    val tokenDefn = Source.fromResource("tokens.txt").mkString
    val scan = Scanner.fromConfig(tokenDefn)
    println(scan.dfa)


    val cfg = Source.fromResource("grammar.cfg").mkString
    println(cfg)
    Jlalr1.parse(cfg)

    if (args.length.equals(0)) {
      println("Must supply a file!")
      return
    }
    println("File given: " + args(0))
  }
}
