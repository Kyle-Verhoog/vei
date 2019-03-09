package compiler.joos1w

import env.Root
import ast.{AST, Weeder}
import compiler.joos1w.env.Root

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
    val weededAsts =
      (files :+ "src/main/resources/AbstractKevin.java").map(file =>
        compileFile(file))

    weededAsts.foreach(ast => {
      Joos1WIfStatement.fixIfs(ast)
    })
    println("Building environments...")
    Joos1WEnvironment.buildEnvironment(weededAsts)
    //println("Other tree...")
    //val root = new Root().populateNamespace(weededAsts)
    //println(root.toStrTree)
    println("done")
  }
}
