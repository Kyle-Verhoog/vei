package compiler

import compiler.ast.{AST, Weeder}
import compiler.parser.{Joos1WParser}
import compiler.scanner.{Joos1WScanner}


object Joos1WCompiler {
  Joos1WScanner.loadSavedScanner()

  def compileFile(file: String) {
    val tokens = Joos1WScanner.scanFile(file)
    val parseTree = Joos1WParser.parse(tokens, file)
    val ast = AST.convertParseTree(parseTree)
    Weeder.weed(ast)
  }
}
