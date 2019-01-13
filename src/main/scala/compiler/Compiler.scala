package compiler

import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  type State = String

  def main(args: Array[String]) {
    val cfg = Source.fromResource("sample.cfg").mkString
    println(cfg)
    Jlalr1.parse(cfg)

    if (args.length.equals(0)) {
      println("Must supply a file!")
      return
    }
    println("File given: " + args(0))
  }
}
