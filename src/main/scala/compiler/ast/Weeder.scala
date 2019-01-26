package compiler.ast

import compiler.ast.literals.{CharacterLiteral, IntegerLiteral, StringLiteral}

import scala.collection.mutable

object Weeder {
  def weed(ast: AST): Unit = {
    ast match {
      case ast: MethodDeclaration => {
        // body is necessarily defined if it has a second child
        val hasBody = ast.body.hasBody
        println("looking at method: " + ast.identifier)
        println(hasBody)
        println(ast.modifiers)
        val isAbstract = ast.modifiers.contains("abstract")
        val abstractOrNative = isAbstract || ast.modifiers.contains("native")

        // rules state it has a body if and only if it is neither abstract nor native
        //println(hasBody)
        //println(abstractOrNative)
        if (hasBody && abstractOrNative) {
          throw SemanticException("Method has body and is abstract/native")
        }

        if (!isAbstract && !hasBody) {
          throw SemanticException("Non-abstract method must have body")
        }
      }
      case ast: CharacterLiteral => {
        val PatternThreeDigitOctal = """'\\[0-3][0-7][0-7](\d{0})'""".r
        val PatternTwoDigitOctal = """'\\[0-7][0-7](\d{0})'""".r
        val PatternOneDigitOctal = """'\\[0-7](\d{0})'""".r

        if (ast.value.length > 3) {
          ast.value match {
            case "'\\b'" | "'\\r'" | "'\\n'" | "'\\t'" | "'\\f'" | "'\\\\'" |
                "'\\\"'" | "'\\\''"        =>
            case PatternThreeDigitOctal(c) => println(c)
            case PatternTwoDigitOctal(c)   =>
            case PatternOneDigitOctal(c)   =>
            case _ =>
              throw SemanticException("Bad escape sequence: " + ast.value)
          }
        }
      }
      case ast: StringLiteral => {
        var i = 0
        while (i < ast.value.length) {
          if (ast.value.charAt(i) == '\\') {
            ast.value.substring(i, i + 2) match {
              case "\\b" | "\\r" | "\\n" | "\\t" | "\\f" | "\\\\" | "\\\"" |
                  "\\\'" | "\\0" | "\\1" | "\\2" | "\\3" | "\\4" | "\\5" |
                  "\\6" | "\\7" =>
              case _ =>
                throw SemanticException(
                  "Bad escape sequence: " + ast.value.substring(i, i + 2))
            }
          }
          i += 1
        }
      }
      case ast: InterfaceDeclaration => {
        val queue = mutable.Queue[Option[AST]](ast.leftChild.get.rightSibling)

        // search for methods, ensure not static or final
        while (queue.nonEmpty) {
          val currentAST = queue.dequeue()
          println("looking at currentAst: " + currentAST)
          if (currentAST.isDefined) {
            currentAST.get match {
              case ast: MethodDeclaration => {
                if (ast.modifiers.contains("static") || ast.modifiers.contains(
                  "final")) {
                  throw SemanticException(
                    "Interface methods cannot be static or final")
                }
              }
              case _ =>
            }
            // recurse
            queue.enqueue(ast.leftChild)
            queue.enqueue(ast.rightSibling)
          }
        }

      }
      case ast: CastExpression => {
        ast.getChild(0).get match {
          case ast: Name =>
          case ast: Identifier =>
          case _ => throw SemanticException("Cast must reduce to identifier")
        }
      }
      case ast: IntegerLiteral => {
        // evaluates the integer literal, causes error to be thrown if its not in range
        ast.integerValue
      }
      case _ =>
    }

    // recurse
    if (ast.leftChild.isDefined) weed(ast.leftChild.get)
    if (ast.rightSibling.isDefined) weed(ast.rightSibling.get)
  }
}
