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
    println(ast, parentEnv)
    ast match {
      case Some(list: ASTList) =>
        if (list.length > 0) {
          list.fieldName match {
            case "formal_parameter_list" =>
              fromAST(list.leftChild, parentEnv)
            case _ => parentEnv
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
      case None => parentEnv
      case _    => parentEnv
      // case Some(ast: AST) =>
      //   parentEnv
      //     .addBlock(fromAST(ast.leftChild, parentEnv))
      //     .addBlock(fromAST(ast.rightSibling, parentEnv))
    }
    // val items: List[PackageItem] = AST.visit(
    //   (ast: Option[AST],
    //    acrossRec: List[PackageItem] => List[PackageItem],
    //    downRec: List[PackageItem] => List[PackageItem]) => {
    //     ast match {
    //       case Some(clsAST: ClassDeclaration) =>
    //         val cls = new Class(this, clsAST)
    //         List(cls)
    //       case Some(intAST: InterfaceDeclaration) =>
    //         val int = new Interface(this, intAST)
    //         List(int)
    //       case _ => downRec(Nil) ++ acrossRec(Nil)
    //     }
    //   },
    //   ast.fold(a => Some(a), a => Some(a)),
    //   List()
    // )
  }

  // Add an AST to this block
  def +(ast: AST): Block = {
    fromAST(Some(ast), this)
  }
}
