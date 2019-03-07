package compiler.joos1w.env

import compiler.joos1w.ast._

class Package(val parent: Root, val ast: Either[PackageDeclaration, Empty])
    extends Env {
  val name: PackageName = ast match {
    case Left(pkgAST) => new PackageName(pkgAST.name)
    case Right(_)     => PackageName.ROOT
  }

  // Gather the imports as a list of qualified names
  // TODO: verify imports
  val imports: List[PackageItemName] = ast match {
    case Left(pkgAST) =>
      pkgAST.rightSibling match {
        case Some(importList: ImportDeclarationsList) =>
          importList.getImports.map(imp =>
            new QualifiedName(imp.name).toPackageItemName)
        case _ => Nil
      }
    case Right(_) => Nil
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

  def importLookup(name: Name): Option[QualifiedName] = {
    None
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
      case pkgName: PackageName   => namespace.get(pkgName)
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
      throw new RuntimeException(
        s"${other.name} $name ${other.name == name}  ${other.name.qualifiedName} ${other.name} ${name.toString} ${other.name.toString} ${other.name
          .hashCode()} ${name.hashCode()}")
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

  // TODO can cache look-ups, as easy as add to namespace??
  def importsLookup(name: Name): Option[Env] = {
    var item: Option[Env] = None
    imports.foreach(imp => {
      // if the import is on-demand then ask the parent for the name
      // with the package prefix
      if (imp.itemName == "*") {
        parent.lookup(imp.packageName + name) match {
          case Some(env: Env) =>
            if (item.isDefined) {
              throw new RuntimeException(
                s"Ambiguous import $item for name $name")
            }
            item = Some(env)
          case _ =>
        }
      }
      // else the name is just the PackageItemName, look it up to get the env
      else if (imp.itemName == name.name) {
        parent.lookup(imp) match {
          case Some(env: Env) =>
            if (item.isDefined) {
              throw new RuntimeException(
                s"Ambiguous import $item for name $name")
            }
            item = Some(env)
          case _ =>
        }
      }
    })
    item
  }

  override def lookup(name: Name): Option[Env] = {
    // 1. search the local package namespace for the name; if that fails
    getItem(name) orElse
      // 2. try the searching the package namespace (via the parent) by qualifying the name
      //    with the package name; if that fails
      parent.lookup(this.name + name) orElse
      // 3. look through the single-type imports for the name; if that fails
      // 4. look through the import-on-demand imports for the name; if that fails
      importsLookup(name) orElse
      // 5. let the parent deal with it... maybe it's in the root package
      parent.lookup(name)
  }
}
