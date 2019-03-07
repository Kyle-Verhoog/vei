package compiler.joos1w.ast

import compiler.joos1w.environment.environment.Signature
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object MethodHeader {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token]): MethodHeader = {
    new MethodHeader(
      modifiers = AST.getValueList(modifiers)
    )
  }

  def validateModifiers(modifiers: List[String]): Unit = {
    // method validation
    if (!(modifiers.contains("public") || modifiers.contains("private") || modifiers
          .contains("protected"))) {
      throw SemanticException(
        "Methods must not be package private (eg. need public/private/protected)"
      )

    }
    if (modifiers.contains("private")) {
      throw SemanticException("Methods cannot be private")
    }
    if (modifiers.contains("abstract")) {
      if (modifiers.contains("static") || modifiers.contains("final")) {
        throw SemanticException(
          "An abstract method cannot be 'static' or 'final'."
        )
      }
    }
    if (modifiers.contains("static")) {
      if (modifiers.contains("final")) {
        throw SemanticException("A static method cannot be 'final'.")
      }
    }
  }
}

class MethodHeader(val modifiers: List[String]) extends AST {
  MethodHeader.validateModifiers(modifiers)
  var currentSignature: Signature = null

  def originalSignature: Signature = {
    (methodDeclarator.identifier,
     Some(parameters.children.map(child =>
       child.asInstanceOf[FormalParameter].ttype.split('.').last)))
  }

  def setSignature(sig: Signature): Unit = {
    currentSignature = sig
  }

  def signature: Signature = {
    if (currentSignature == null) {
      originalSignature
    } else {
      currentSignature
    }
  }

  def params: List[(String, String)] = {
    var ps: List[(String, String)] = List()
    parameters.children.foreach((c: AST) =>
      c match {
        case c: FormalParameter => ps = (c.ttype, c.name) :: ps
        case _                  => throw new RuntimeException("should not happen")
    })
    ps
  }

  def parameters: ASTList = {
    methodDeclarator.parameters
  }

  def methodDeclarator: MethodDeclarator = {
    children(1).asInstanceOf[MethodDeclarator]
  }

  def returnType: String = {
    if (children.isEmpty) return "void" // TODO void
    getChild(0) match {
      case Some(t: Type) => t.ttype
      case _             => throw new RuntimeException("")
    }
  }

  override def strFields: String = {
    s"$returnType"
  }
}
