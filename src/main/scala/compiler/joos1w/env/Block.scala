package compiler.joos1w.env

import compiler.joos1w.ast._

object Block {
  def mkEmptyBlock(parent: Env): Block = {
    new Block(parent)
  }
}

class Block(var parent: Env, val ast: AST = new Empty) extends Env {
  type Namespace = Array[Block]
  var namespace: Namespace = Array()
  val name: Option[VariableName] = ast match {
    case formalParameter: FormalParameter =>
      Some(new VariableName(formalParameter.ttype, formalParameter.name))
    case localVariableDeclaration: LocalVariableDeclaration =>
      Some(
        new VariableName(localVariableDeclaration.ttype,
                         localVariableDeclaration.name))
    case _ => None
  }

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    // search namespace, if nothing, then parent
    parent.lookup(name)
  }

  override def toString: String = {
    s"Block($name)"
  }

  def addBlock(block: Block): Block = {
    block.parent = this
    namespace = namespace :+ block
    this
  }

  override def toStrTree: String = {
    val cs: List[String] = namespace.toList
      .map(block => {
        val childStrs = block.toStrTree.split("\n")
        val tailChar = if (childStrs.tail.isEmpty) "" else "\n"
        s"┠─ " + childStrs.head + tailChar + childStrs.tail
          .map(
            line => "┃  " + line
          )
          .mkString("\n")
      })
    val scs = cs.mkString("\n")
    s"$toString\n$scs"
  }

  def fromAST(ast: Option[AST], parentEnv: Block): Block = {
    // println(ast, parentEnv)
    ast match {
      case Some(list: ASTList) =>
        if (list.length > 0) {
          list.fieldName match {
            case "block_statements" =>
              val block = new Block(parentEnv)
              parentEnv.addBlock(block)
              fromAST(list.leftChild, block)
              fromAST(list.rightSibling, parentEnv)
            case _ =>
              fromAST(list.leftChild, parentEnv)
              fromAST(list.rightSibling, parentEnv)
          }
        } else {
          fromAST(list.rightSibling, parentEnv)
        }
      case Some(formalParameter: FormalParameter) =>
        val block = new Block(parentEnv, formalParameter)
        parentEnv.addBlock(block)
        fromAST(formalParameter.rightSibling, block)
      case Some(methodDeclarator: MethodDeclarator) =>
        fromAST(methodDeclarator.leftChild, parentEnv)
      case Some(methodBody: MethodBody) =>
        fromAST(methodBody.leftChild, parentEnv)
      case Some(localVariableDeclaration: LocalVariableDeclaration) =>
        val block = new Block(parentEnv, localVariableDeclaration)
        parentEnv.addBlock(block)
        fromAST(localVariableDeclaration.rightSibling, block)
      case Some(forStatement: ForStatement) =>
        if (forStatement.hasBody) {
          val block = if (forStatement.hasDeclaration) {
            new Block(parentEnv, forStatement.initialization)
          } else {
            new Block(parentEnv, forStatement.initialization)
          }
          parentEnv.addBlock(block)
          fromAST(Some(forStatement.body), block)
        }
        fromAST(forStatement.rightSibling, parentEnv)
      case None => parentEnv
      case Some(n: AST) =>
        fromAST(n.leftChild, parentEnv)
        fromAST(n.rightSibling, parentEnv)
    }
  }

  // Add an AST to this block
  def +(ast: AST): Block = {
    fromAST(Some(ast), this)
  }
}
