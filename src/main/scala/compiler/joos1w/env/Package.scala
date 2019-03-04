package compiler.joos1w.env

import compiler.joos1w.ast.{AST, ClassDeclaration, PackageDeclaration}

object Package {
  def fromAST(ast: PackageDeclaration): Package = {
    val name = ast.name
    val pkg = new Package(name, Map())
    pkg.fromAST(Some(ast))
  }
}

class Package(val name: String, val classes: Map[String, Class]) extends Env {
  def addClass(cls: Class): Package = {
    val newClasses = classes + (cls.name -> cls)
    new Package(
      name,
      newClasses,
    )
  }

  def numClasses: Int = {
    classes.size
  }

  def hasClass(name: String): Boolean = {
    classes.contains(name)
  }

  def getClass(name: String): Class = {
    classes(name)
  }

  def +(other: Package): Package = {
    if (other.name != name) {
      throw new RuntimeException()
    }

    new Package(
      name,
      classes ++ other.classes,
    )
  }

  def fromAST(ast: Option[AST]): Package = {
    ast match {
      case Some(ast) =>
        ast match {
          case ast: ClassDeclaration =>
            val pkg = fromAST(ast.rightSibling)
            val cls = Class.fromAST(this, ast)
            pkg.addClass(cls)
          case _ =>
            val nodes = ast.rightSibling :: ast.children.map(c => Option(c))
            val results = nodes.foldLeft[Package](this) {
              (prev, ast) =>
                val pkg = fromAST(ast)
                prev + pkg
            }
            results
        }
      case None => this
    }
  }
}