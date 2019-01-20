package compiler.ast

object AST {

  class AST(var parent: Option[AST] = None,
            var leftChild: Option[AST] = None,
            var rightSibling: Option[AST] = None) {

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
}