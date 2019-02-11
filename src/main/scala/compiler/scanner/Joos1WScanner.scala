package compiler.scanner

import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.{File, PrintWriter}

import regex.Regex

object Joos1WScanner {
  var scanner: Option[Scanner] = None
  val alphabet: Set[String] = Regex.asciiAlphabet | Set("‹", "›")

  def generateNewScanner(
      configFile: String = Source.fromResource("tokens.lex").mkString): Unit = {
    scanner = Some(Scanner.fromConfig(configFile, alphabet))
  }

  def loadSavedScanner(
      rawDFA: String = Source.fromResource("serializations/dfa").mkString)
    : Unit = {
    scanner = Some(Scanner.fromSerializedDFA(rawDFA))
  }

  def saveScanner(dfaFileName: String): Unit = {
    val serializedDFA = Scanner.serializeDFA(scanner.get.dfa, dfaFileName)
    val file = new File(dfaFileName)
    println("writing to " + dfaFileName)
    val pw = new PrintWriter(file)
    pw.write(serializedDFA)
    pw.close()
  }

  def scan(src: String,
           fileName: Option[String] = None,
           keepWhitespace: Boolean = false,
           keepComments: Boolean = false): ListBuffer[Token] = {
    val scanr = scanner.get
    var tokens = scanr.scan(escapeComments(src), fileName)
    if (!keepComments) {
      tokens = removeCommentTokens(tokens)
    }
    if (!keepWhitespace) {
      tokens = removeWhitespaceTokens(tokens)
    }
    tokens.prepend(new Token("BOF", "bof"))
    tokens.append(new Token("EOF", "eof"))
    tokens
  }

  def scanFile(fileName: String): ListBuffer[Token] = {
    val src = Source.fromFile(fileName).mkString
    scan(src, Some(fileName))
  }

  def escapeComments(input: String): String = {
    val s = input.replaceAll("[^\\*]//\\*", s"// *")
    s.replaceAll("((?s)/\\*.*?\\*/)", s"‹$$1›")
  }

  def removeCommentTokens(tokens: ListBuffer[Token]): ListBuffer[Token] = {
    tokens.filter(t =>
      !Set("LINE_COMMENT", "MULTILINE_COMMENT").contains(t.tokenType))
  }

  def removeWhitespaceTokens(tokens: ListBuffer[Token]): ListBuffer[Token] = {
    tokens.filter(t => !Set("NEWLINE", "WHITESPACE").contains(t.tokenType))
  }
}
