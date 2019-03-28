package compiler.joos1w

import ast.{AST, Weeder}
import java.io._
import java.nio._

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

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
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
    println("Building environments...")
    //println("Other tree...")
    //val root = new Root().populateNamespace(weededAsts)
    //println(root.toStrTree)
    //Joos1WReachability.checkReachability(weededAsts)
    val asm = Joos1WCodeGen.genCode(weededAsts)

    val pwd = System.getenv("PWD")
    println(s"OUTPUTTING TO $pwd")
    val outputDir = pwd ++ "/output"
    asm.foreach(code => {
      println(s"OUTPUTTING CODE ${code.fileName}")
      val fileName = outputDir ++ "/" ++ code.fileName
      printToFile(new File(fileName)) { p =>
        p.print(code.src)
      }
    })

    println("done")
  }
}
