package compiler.joos1w

import compiler.joos1w.ast.{AST, Weeder}

object Joos1WCompiler {
  Joos1WScanner.loadSavedScanner()

  def compileFile(file: String) {
    println(s"Compiling file: $file")
    println("Scanning...")
    val tokens = Joos1WScanner.scanFile(file)
    println("Parsing...")
    val parseTree = Joos1WParser.parse(tokens, file)
    println("Generating AST...")
    val ast = AST.convertParseTree(parseTree)
    println("Weeding...")
    Weeder.weed(ast)
  }
}
