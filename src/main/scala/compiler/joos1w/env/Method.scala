package compiler.joos1w.env

import compiler.joos1w.ast._

abstract class GeneralMethod(parent: PackageItem, ast: ASTMethodDeclaration)
    extends ClassItem(parent, ast) {
  type Namespace = Block
  var namespace: Namespace = Block.mkEmptyBlock(this)

  val name: MethodName

  def populateNamespace(ast: ASTMethodDeclaration = ast): GeneralMethod = {
    // add parameters to method block
    var block = namespace + ast.header.get.methodDeclarator
    block = block + ast.body
    this
  }

  override def toStrTree: String = {
    val cs: List[String] = List(namespace)
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

  override def toString: String = {
    s"Method(name: $name)"
  }

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    parent.lookup(name)
  }
}

class Method(parent: PackageItem, ast: ASTMethodDeclaration)
    extends GeneralMethod(parent, ast) {
  val name: MethodName = ast match {
    case _: MethodDeclaration | _: AbstractMethodDeclaration =>
      new MethodName(ast.modifiers,
                     ast.returnType,
                     ast.identifier,
                     ast.header.get.params)
    case _ => throw new RuntimeException(s"Unexpected method ast node $ast")
  }
}

class Constructor(parent: PackageItem, ast: ConstructorDeclaration)
    extends GeneralMethod(parent, ast) {
  override val name: ConstructorName = new ConstructorName(
    ast.modifiers,
    ast.returnType,
    ast.identifier,
    ast.constructorDeclarator.params
  )
}
