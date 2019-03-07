package compiler.joos1w.env

import compiler.joos1w.ast.AST

abstract class ClassItem(var parent: PackageItem, ast: AST) extends Env {
  val name: ClassItemName

  // type Namespace = Map[, ClassItem]
  // var namespace: Namespace = Map()

  // def populateNamespace(ast: AST = ast): PackageItem = {
  //   val items: List[ClassItem] = AST.visit(
  //     (ast: Option[AST],
  //      acrossRec: List[ClassItem] => List[ClassItem],
  //      downRec: List[ClassItem] => List[ClassItem]) => {
  //       ast match {
  //         case Some(methAST: MethodDeclaration) =>
  //           new Method(this, methAST) :: acrossRec(Nil)
  //         case Some(amethAST: AbstractMethodDeclaration) =>
  //           new Method(this, amethAST) :: acrossRec(Nil)
  //         case Some(consAST: ConstructorDeclaration) =>
  //           new Constructor(this, consAST) :: acrossRec(Nil)
  //         case Some(fieldAST: FieldDeclaration) =>
  //           new Field(this, fieldAST) :: acrossRec(Nil)
  //         case _ => downRec(Nil) ++ acrossRec(Nil)
  //       }
  //     },
  //     Some(ast),
  //     Nil
  //   )
  //   items.foreach(item => {
  //     println(item.toStrTree)
  //   })
  //   this
  // }

  override def toStrTree: String = {
    s"ClassItem(name: $name)"
  }
}
