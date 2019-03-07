package compiler.joos1w.env

import compiler.joos1w.ast._

abstract class PackageItem(var parent: Package, ast: AST) extends Env {
  val name: PackageItemName

  type Namespace = Map[ClassItemName, ClassItem]
  var namespace: Namespace = Map()

  def addField(field: Field): PackageItem = {
    // TODO field-specific logic
    namespace = namespace + (field.name -> field)
    this
  }

  def addMethod(method: Method): PackageItem = {
    // TODO method-specific logic
    namespace = namespace + (method.name -> method)
    this
  }

  def addConstructor(constructor: Constructor): PackageItem = {
    // TODO constructor-specific logic
    namespace = namespace + (constructor.name -> constructor)
    this
  }

  def allItems: List[ClassItem] = {
    namespace.foldLeft(Nil: List[ClassItem]) {
      case (acc, (_, cls)) =>
        cls :: acc
    }
  }

  def populateNamespace(ast: AST = ast): PackageItem = {
    val items: List[ClassItem] = AST.visit(
      (ast: Option[AST],
       acrossRec: List[ClassItem] => List[ClassItem],
       downRec: List[ClassItem] => List[ClassItem]) => {
        ast match {
          case Some(methAST: MethodDeclaration) =>
            new Method(this, methAST) :: acrossRec(Nil)
          case Some(amethAST: AbstractMethodDeclaration) =>
            new Method(this, amethAST) :: acrossRec(Nil)
          case Some(consAST: ConstructorDeclaration) =>
            new Constructor(this, consAST) :: acrossRec(Nil)
          case Some(fieldAST: FieldDeclaration) =>
            new Field(this, fieldAST) :: acrossRec(Nil)
          case _ => downRec(Nil) ++ acrossRec(Nil)
        }
      },
      Some(ast),
      Nil
    )

    items.foreach({
      case method: Method =>
        addMethod(method)
      case constructor: Constructor =>
        addConstructor(constructor)
      case field: Field =>
        addField(field)
    })
    this
  }

  override def toStrTree: String = {
    val cs: List[String] = namespace.toList
      .map({
        case (_: ClassItemName, item: ClassItem) =>
          val childStrs = item.toStrTree.split("\n")
          val tailChar = if (childStrs.tail.isEmpty) "" else "\n"
          s"┠─ " + childStrs.head + tailChar + childStrs.tail
            .map(
              line => "┃  " + line
            )
            .mkString("\n")
        case _ => ""
      })
    val scs = cs.mkString("\n")
    s"$toString\n$scs"
  }
}
