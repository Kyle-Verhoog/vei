package compiler

import compiler.ast.{AST, Weeder}
import compiler.parser.Parser
import compiler.scanner.{Joos1WScanner, Scanner, Token}
import jlalr.Jlalr1

import scala.io.Source

object Compiler {
  def main(args: Array[String]) {
    //serialize()
    //generateTableLR1()
    //runActual(args)
    runTestFile()
  }

  def runTestFile(testFile: String = "test/Empty.java"): Unit = {
    val file = Source.fromResource(testFile).mkString
    println("Running file: " + testFile)
    println("Scanning...")
    // Joos1WScanner.generateNewScanner()
    // Joos1WScanner.saveScanner("dfa")
    Joos1WScanner.loadSavedScanner()
    val tokens = Joos1WScanner.scanFile("src/main/resources/test/Empty.java")

    tokens.prepend(new Token("BOF", "bof"))
    tokens.append(new Token("EOF", "eof"))

    println("Parsing...")
    val cfg =
      Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
    val parseTree = Parser.parse(cfg, tokens)

    println("Converting to ast....")
    val ast = AST.convertParseTree(parseTree(1))
    println("Weeding...")
    Weeder.weed(ast)
    println("Building environment...")
    val env = environment.buildEnvironment(ast, None)
    environment.verifyEnvironment(env)
  }

  def runActual(args: Array[String]): Unit = {
    if (args.length.equals(0)) {
      println("Must supply a file!")
      System.exit(1) // TODO error codes?
    }

    try {
      val file = Source.fromFile(args(0)).mkString
      println("Read in file: " + file)
      println("Scanning...")
      // val tokens = scanWithoutSerializing(file)

      // add BOF and EOF for parsing
      // tokens.prepend(new Token("BOF", "bof"))
      // tokens.append(new Token("EOF", "eof"))

      println("Parsing...")
      val cfg =
        Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
      // val parseTree = Parser.parse(cfg, tokens)

      println("Converting to ast....")
      // val ast = AST.convertParseTree(parseTree(1))
      // println(ast)
      println("Weeding...")
      // Weeder.weed(ast)
    } catch {
      case e: Exception =>
        println(e)
        System.exit(42)
    }
    System.exit(0)
  }

  def generateTableLALR1(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    Jlalr1.parse(cfg)
  }

  def generateTableLR1(): Unit = {
    val cfg = Source.fromResource("grammar.cfg").mkString
    Jlalr1.parseLr1(cfg)
  }
}
