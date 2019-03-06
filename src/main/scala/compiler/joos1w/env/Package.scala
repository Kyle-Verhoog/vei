package compiler.joos1w.env

import compiler.joos1w.ast.{AST, Empty, ClassDeclaration, PackageDeclaration}

object Package {}

class Package(val parent: Root, val ast: Either[PackageDeclaration, Empty])
    extends Env {
  val name: PackageName = ast match {
    case Left(pkgAST)    => new PackageName(pkgAST.name)
    case Right(emptyAST) => PackageName.ROOT
  }

  type Namespace = Map[Name, Class]
  var namespace: Namespace =
    AST.foldUp[ClassDeclaration, Namespace](
      (ast, accMap) => {
        val cls = new Class(this, ast)
        accMap + (cls.name -> cls)
      },
      ast.fold(a => Some(a), a => Some(a)),
      Map(),
      AST.RecursionOptions(true, true, true, (m1, m2) => m1 ++ m2)
    )

  def addClass(cls: Class): Package = {
    namespace = namespace + (cls.name -> cls)
    this
  }

  def numClasses: Int = {
    namespace.size
  }

  def hasClass(name: Name): Boolean = {
    namespace.contains(name)
  }

  def getClass(name: Name): Option[Class] = {
    if (hasClass(name)) Some(namespace(name)) else None
  }

  def getAllClasses: List[Class] = {
    namespace.foldLeft(Nil: List[Class]) {
      case (acc, (_, cls)) =>
        cls :: acc
    }
  }

  def +(other: Package): Package = {
    if (other.name != name) {
      throw new RuntimeException()
    }
    namespace = namespace ++ other.namespace
    this
  }

  override def toString: String = {
    s"Package(name: $name, nitems: ${namespace.size})"
  }

  override def toStrTree: String = {
    val cs: List[String] = namespace.toList
      .map({
        case (_: Name, cls: Class) =>
          val childStrs = cls.toStrTree.split("\n")
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

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    parent.lookup(name)
  }
}
