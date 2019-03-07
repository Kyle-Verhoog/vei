package compiler.joos1w.env

import compiler.joos1w.ast._

object Package {}

class Package(val parent: Root, val ast: Either[PackageDeclaration, Empty])
    extends Env {
  val name: PackageName = ast match {
    case Left(pkgAST)    => new PackageName(pkgAST.name)
    case Right(emptyAST) => PackageName.ROOT
  }

  type Namespace = Map[Name, PackageItem]
  var namespace: Namespace = Map()

  def addClass(cls: Class): Package = {
    namespace = namespace + (cls.name -> cls)
    this
  }

  def hasClass(name: ClassName): Boolean = {
    namespace.contains(name)
  }

  def populateNamespace: Package = {
    val items: List[PackageItem] = AST.visit(
      (ast: Option[AST],
       acrossRec: List[PackageItem] => List[PackageItem],
       downRec: List[PackageItem] => List[PackageItem]) => {
        ast match {
          case Some(clsAST: ClassDeclaration) =>
            val cls = new Class(this, clsAST)
            List(cls)
          case Some(intAST: InterfaceDeclaration) =>
            val int = new Interface(this, intAST)
            List(int)
          case _ => downRec(Nil) ++ acrossRec(Nil)
        }
      },
      ast.fold(a => Some(a), a => Some(a)),
      List()
    )

    items.foreach(item => {
      namespace = namespace + (item.name -> item)
    })
    this
  }

  def getItem(name: Name): Option[PackageItem] = {
    name match {
      case clsName: ClassName     => namespace.get(clsName)
      case intName: InterfaceName => namespace.get(intName)
      case name: Name =>
        val clsName = ClassName(this.name, name.name)
        val intName = InterfaceName(this.name, name.name)
        if (namespace.contains(clsName))
          Some(namespace(clsName))
        else if (namespace.contains(intName))
          Some(namespace(intName))
        else
          None
    }
  }

  def allItems: List[PackageItem] = {
    namespace.foldLeft(Nil: List[PackageItem]) {
      case (acc, (_, cls)) =>
        cls :: acc
    }
  }

  def +(other: Package): Package = {
    if (other.name != name) {
      throw new RuntimeException()
    }

    other.namespace.foreach({
      case (name: Name, item: PackageItem) =>
        item.parent = this // update parent reference
        namespace = namespace + (name -> item)
    })
    namespace = namespace ++ other.namespace
    this
  }

  override def toString: String = {
    s"Package(name: $name, nitems: ${namespace.size})"
  }

  override def toStrTree: String = {
    val cs: List[String] = namespace.toList
      .map({
        case (_: Name, item: PackageItem) =>
          val childStrs = item.toStrTree.split("\n")
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
    getItem(name) match {
      case Some(item) => Some(item)
      case None       => parent.lookup(name.toQualifiedName)
    }
  }
}
