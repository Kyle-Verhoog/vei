package compiler.ast

class AST(var parent: Option[AST], var leftChild: Option[AST], var rightSibling: Option[AST]) {

  // adds sibling to end of sibling list and sets siblings parent accordingly
  def addSiblingToEnd(sib: AST): Unit = {
    rightSibling match {
      case Some(node) => node.addSiblingToEnd(sib)
      case None => {
        sib.parent = parent
        rightSibling = Some(sib)
      }
    }
  }

}
