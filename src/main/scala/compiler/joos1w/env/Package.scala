package compiler.joos1w.env

import compiler.joos1w.ast.{AST, ClassDeclaration, PackageDeclaration}

object Package {}

class Package(val parent: Root, val ast: PackageDeclaration) extends Env {
  val name: PackageName = new PackageName(ast.name)

  type ClassMap = Map[Name, Class]
  var classes: ClassMap =
    AST.foldUp[ClassDeclaration, ClassMap](
      (ast, accMap) => {
        val cls = new Class(this, ast)
        accMap + (cls.name -> cls)
      },
      Some(ast),
      Map(),
      AST.RecursionOptions(true, true, true, (m1, m2) => m1 ++ m2)
    )

  def addClass(cls: Class): Package = {
    classes = classes + (cls.name -> cls)
    this
  }

  def numClasses: Int = {
    classes.size
  }

  def hasClass(name: Name): Boolean = {
    println(name, classes)
    classes.contains(name)
  }

  def getClass(name: Name): Option[Class] = {
    if (hasClass(name)) Some(classes(name)) else None
  }

  def getAllClasses: List[Class] = {
    classes.foldLeft(Nil: List[Class]) {
      case (acc, (_, cls)) =>
        cls :: acc
    }
  }

  def +(other: Package): Package = {
    if (other.name != name) {
      throw new RuntimeException()
    }
    classes = classes ++ other.classes
    this
  }

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    parent.lookup(name)
  }
}
