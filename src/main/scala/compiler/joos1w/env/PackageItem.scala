package compiler.joos1w.env

import compiler.joos1w.ast._

abstract class PackageItem(var parent: Package, ast: AST) extends Env {
  val name: PackageItemName

  type Namespace = Map[ClassItemName, ClassItem]
  var namespace: Namespace = Map()

  def populateNamespace(ast: AST = ast): PackageItem = {
    val items: List[ClassItem] = AST.visit(
      (ast: Option[AST],
       acrossRec: List[ClassItem] => List[ClassItem],
       downRec: List[ClassItem] => List[ClassItem]) => {
        ast match {
          case Some(methAST: MethodDeclaration) =>
            List(new Method(this, methAST))
          case Some(amethAST: AbstractMethodDeclaration) =>
            List(new Method(this, amethAST))
          case Some(consAST: ConstructorDeclaration) =>
            List(new Constructor(this, consAST))
          case Some(fieldAST: FieldDeclaration) =>
            List(new Field(this, fieldAST))
          case _ => downRec(Nil) ++ acrossRec(Nil)
        }
      },
      Some(ast),
      List()
    )
    this
  }
}
