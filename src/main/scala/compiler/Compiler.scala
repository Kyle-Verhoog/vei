package compiler

import compiler.ast.{AST, Weeder}
import compiler.parser.Parser
import compiler.scanner.{Joos1WScanner, Scanner, Token}
import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  def main(args: Array[String]) {
    // Joos1WScanner.generateNewScanner()
    // Joos1WScanner.saveScanner("src/main/resources/serializations/dfa")
    //generateTableLR1()
    runActual(args)
    // runTestFile()
  }

  def runTestFile(
      testFile: String = "src/main/resources/test/Empty.java"): Unit = {
    val file = Source.fromFile(testFile).mkString
    println("Running file: " + testFile)
    println("Scanning...")
    // Joos1WScanner.generateNewScanner()
    // Joos1WScanner.saveScanner("dfa")
    Joos1WScanner.loadSavedScanner()
    val tokens = Joos1WScanner.scanFile(testFile)

    println("Parsing...")
    val cfg =
      Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
    val parseTree = Parser.parse(cfg, tokens, testFile)

    println("Converting to ast....")
    val ast = AST.convertParseTree(parseTree(1))
    println("Weeding...")
    Weeder.weed(ast)
    println("Building environment...")
    //val env = environment.buildEnvironment(ast, None)
    //environment.verifyEnvironment(env)
  }

  def runActual(args: Array[String]): Unit = {
    if (args.length.equals(0)) {
      println("Must supply a file!")
      System.exit(1) // TODO error codes?
    }

    try {
      runTestFile(args(0))
    } catch {
      case e: Exception =>
        println(e)
        System.exit(42)
    }
    System.exit(0)
  }

  def generateTableLR1(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    Jlalr1.parseLr1(cfg)
  }
}
