package compiler

object Compiler {
  def main(args:Array[String]) {
    if (args.length.equals(0)) {
      println("Must supply a file!")
      return
    }

    println("File given: " + args(0))
    val lex = new Lexer
  }
}