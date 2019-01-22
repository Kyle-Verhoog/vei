package compiler.scanner

import scala.collection.mutable.ListBuffer
import scala.io.Source

import java.io.{File, PrintWriter}

object Joos1WScanner {
  var scanner: Option[Scanner] = None

  def generateNewScanner(
      configFile: String = Source.fromResource("tokens.lex").mkString): Unit = {
    scanner = Some(Scanner.fromConfig(configFile))
  }

  def loadSavedScanner(dfaFile: String =
    Source.fromResource("serializations/dfa_serialization").mkString): Unit = {
    scanner = Some(Scanner.fromSerializedDFA(dfaFile))
  }

  def saveScanner(dfaFileName: String): Unit = {
    println("\n\n\n")
    println(scanner.get.dfa.alphabet.contains("›"))
    println("\n\n\n")
    val serializedDFA = Scanner.serializeDFA(scanner.get.dfa, dfaFileName)
    val file = new File(dfaFileName)
    println("writing to " + dfaFileName)
    val pw = new PrintWriter(file)
    pw.write(serializedDFA)
    pw.close()
  }

  def scan(src: String, fileName: Option[String] = None): ListBuffer[Token] = {
    val scanr = scanner.get
    var tokens = scanr.scan(removeComments(src), fileName)
    tokens = removeCommentTokens(tokens)
    tokens = removeWhitespaceTokens(tokens)
    tokens.prepend(new Token("BOF", "bof"))
    tokens.append(new Token("EOF", "eof"))
    tokens
  }

  def scanFile(fileName: String): ListBuffer[Token] = {
    val src = Source.fromFile(fileName).mkString
    scan(src, Some(fileName))
  }

  def removeComments(input: String): String = {
    val escaped =
      input.replaceAllLiterally("/*", "‹").replaceAllLiterally("*/", "›")
    escaped
  }

  def removeCommentTokens(tokens: ListBuffer[Token]): ListBuffer[Token] = {
    tokens.filter(t =>
      !Set("LINE_COMMENT", "MULTILINE_COMMENT").contains(t.tokenType))
  }

  def removeWhitespaceTokens(tokens: ListBuffer[Token]): ListBuffer[Token] = {
    tokens.filter(t => !Set("NEWLINE", "WHITESPACE").contains(t.tokenType))
  }
}
