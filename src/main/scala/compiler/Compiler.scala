package compiler

import compiler.ast.AST
import compiler.parser.Parser
import compiler.scanner.{Scanner, Token}
import jlalr.Jlalr1

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Compiler {
  def main(args: Array[String]) {
    serialize()
    //generateTableLR1()
    //runActual(args)
    //runTestFile()
  }

  def runTestFile(testFile: String = "test/Empty.java"): Unit = {
    val file = Source.fromResource(testFile).mkString
    println("Running file: " + testFile)
    println("Scanning...")
    val tokens = scanWithoutSerializing(file)

    // add BOF and EOF for parsing
    tokens.prepend(new Token("BOF", "bof"))
    tokens.append(new Token("EOF", "eof"))

    println("Parsing...")
    val cfg =
      Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
    val parseTree = Parser.parse(cfg, tokens)

    println("Converting to ast....")
    val ast = AST.convertParseTree(parseTree(1))
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
      val tokens = scanWithoutSerializing(file)

      // add BOF and EOF for parsing
      tokens.prepend(new Token("BOF", "bof"))
      tokens.append(new Token("EOF", "eof"))

      println("Parsing...")
      val cfg =
        Parser.readInLr1(Source.fromResource("grammar.lr1").getLines().toArray)
      val parseTree = Parser.parse(cfg, tokens)

      println("Converting to ast....")
      val ast = AST.convertParseTree(parseTree(1))
      println(ast)
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

  def serialize(): Unit = {
    val tokenDefn = Source.fromResource("tokens.lex").mkString
    val scan = Scanner.fromConfig(tokenDefn)
    Scanner.serializeDfa(scan.dfa, "dfa_serialization")
  }

  def scanWithoutSerializing(
      fileContents: String = Source.fromResource("test/Empty.java").mkString)
    : ListBuffer[Token] = {
    val fileSansCOmments = removeComments(fileContents)
    //println("without " + fileSansCOmments)

    val scan = new Scanner()
    scan
      .scan(fileSansCOmments)
      .filter(
        token =>
          !token.tokenType.equals("NEWLINE") && !token.tokenType
            .equals("WHITESPACE")
      )
  }

  def removeComments(input: String): String = {
    var parsedProg = ""
    var i = 0
    while (i < input.toCharArray.length) {
      if (input(i) == '/' && (i + 1 < input.toCharArray.length && input(i + 1) == '/')) { // remove single line comments
        while (input(i) != '\n') {
          i += 1
        }
      } else if (input(i) == '/' && (i + 1 < input.toCharArray.length && input(
                   i + 1) == '*')) {
        i += 2 // remove starting star
        // if this fails because we go out of bounds, thats fine since comments should finish
        while (!(input(i) == '*' && input(i + 1) == '/')) {
          i += 1
        }
        i += 2 // remove ending star
      } else {
        parsedProg += input(i)
        i += 1
      }
    }
    parsedProg
  }
}
