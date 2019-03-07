package compiler.joos1w.ast

class ForStatement() extends AST {
  def hasDeclaration: Boolean = {
    getChild(0) match {
      case Some(_: LocalVariableDeclaration) =>
        true
      case _ => false
    }
  }

  def initialization: AST = {
    getChild(0).get
  }

  def termination: AST = {
    getChild(1).get
  }

  def increment: AST = {
    getChild(2).get
  }

  def hasBody: Boolean = {
    getChild(3) match {
      case Some(_: Empty) => false
      case _              => true
    }
  }

  def body: AST = {
    getChild(3).get
  }
}
