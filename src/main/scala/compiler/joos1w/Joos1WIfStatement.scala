package compiler.joos1w

import compiler.joos1w.ast.{AST, IfStatement, TopLevelIf}

object Joos1WIfStatement {

  def fixIfs(ast: AST): Unit = {
    ast.rightSibling match {
      case Some(ifAst: IfStatement) => {
        val top = new TopLevelIf()
        top.rightSibling = ifAst.rightSibling

        val childrenStatements = ifAst.getTopLevelStatements()
        for (i <- childrenStatements.indices) {
          if (i < childrenStatements.length - 1) {
            childrenStatements(i).rightSibling = Some(childrenStatements(i + 1))
          } else {
            childrenStatements(i).rightSibling = None
          }
        }

        top.leftChild = Some(childrenStatements.head)
        ast.rightSibling = Some(top)

        fixIfs(top)
      }
      case Some(rightSibling: AST) => fixIfs(rightSibling)
      case _              =>
    }

    if (!ast.isInstanceOf[TopLevelIf]) {
      ast.leftChild match {
        case Some(ifAst: IfStatement) => {
          val top = new TopLevelIf()
          top.rightSibling = ifAst.rightSibling

          val childrenStatements = ifAst.getTopLevelStatements()
          for (i <- childrenStatements.indices) {
            if (i < childrenStatements.length - 1) {
              childrenStatements(i).rightSibling = Some(
                childrenStatements(i + 1))
            } else {
              childrenStatements(i).rightSibling = None
            }
          }

          top.leftChild = Some(childrenStatements.head)
          ast.leftChild = Some(top)
          top.parent = Some(ast)

          fixIfs(top)
        }
        case Some(leftChild: AST) => fixIfs(leftChild)
        case _              =>
      }
    } else {
      ast.children.foreach(child => fixIfs(child))
    }
  }
}
