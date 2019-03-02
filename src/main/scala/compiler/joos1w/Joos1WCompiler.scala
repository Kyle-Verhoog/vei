package compiler.joos1w

import compiler.joos1w.ast.{AST, Weeder}
import compiler.joos1w.environment.RootEnvironment

object Joos1WCompiler {
  Joos1WScanner.loadSavedScanner()

  def compileFile(file: String): AST = {
    //println(s"Compiling file: $file")
    //println("Scanning...")
    val tokens = Joos1WScanner.scanFile(file)
    //println("Parsing...")
    val parseTree = Joos1WParser.parse(tokens, file)
    //println("Generating AST...")
    val ast = AST.fromParseTree(parseTree)
    //println("Weeding...")
    Weeder.weed(ast)
    ast
  }

  def compileFiles(files: List[String]): Unit = {
    println("Scanning, parsing, weeding files...")
    val weededAst = files.map(file => compileFile(file))
    println("Building environments...")
    val rootEnvironment = new RootEnvironment(new AST(), None)

    weededAst.foreach(ast =>
      environment.environment.buildEnvironment(ast, Option(rootEnvironment)))
    environment.environment.verifyEnvironment(rootEnvironment)
    println("done")
  }
}
