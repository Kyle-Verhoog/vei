package compiler.ast

import compiler.ast.literals.{CharacterLiteral, StringLiteral}

object Weeder {
  def weed(ast: AST): Unit = {
    ast match {
      case ast: MethodDeclaration => {
        // body is necessarily defined if it has a second child
        val hasBody = ast.leftChild.get.rightSibling.isDefined
        val header = ast.leftChild.get.asInstanceOf[MethodHeader]
        val abstractOrNative = header.modifiers.contains("abstract") || header.modifiers
          .contains("native")

        // rules state it has a body if and only if it is neither abstract nor native
        if (hasBody) {
          if (abstractOrNative) {
            throw SemanticException("Method has body and is abstract/native")
          }
        } else {
          if (!abstractOrNative)
            throw SemanticException(
              "Method has no body, so it must be abstract or native")
        }
      }
      case ast: CharacterLiteral => {
        if (ast.value.length > 3) {
          ast.value match {
            case "'\\b'" | "'\\r'" | "'\\n'" | "'\\t'" | "'\\f'" | "'\\\\'" |
                "'\\\"'" =>
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
              case "\\b" | "\\r" | "\\n" | "\\t" | "\\f" | "\\\\" | "\\\"" =>
              case _ =>
                throw SemanticException("Bad escape sequence: " + ast.value.substring(i, i + 2))
            }
          }
          i += 1
        }
      }
      case _ =>
    }

    // recurse
    if (ast.leftChild.isDefined) weed(ast.leftChild.get)
    if (ast.rightSibling.isDefined) weed(ast.rightSibling.get)
  }
}
