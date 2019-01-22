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

  def removeCommentTokens(tokens: ListBuffer[Token]): ListBuffer[Token] = {
    val newTokens = ListBuffer[Token]()
    var i = 0
    while (i < tokens.length) {
      var token = tokens(i)

      if (token.tokenType != "LINE_COMMENT" && token.tokenType != "START_MULTI_COMMENT") {
        newTokens.append(token)
      } else if (token.tokenType == "START_MULTI_COMMENT") {
        while (token.tokenType != "END_MULTI_COMMENT") {
          i += 1
          token = tokens(i)
        }
      }
      i += 1
    }

    newTokens
  }

  def removeWhitespaceTokens(tokens: ListBuffer[Token]): ListBuffer[Token] = {
    tokens.filter(t => !Set("NEWLINE", "WHITESPACE").contains(t.tokenType))
  }
}
