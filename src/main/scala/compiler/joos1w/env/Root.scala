package compiler.joos1w.env

import compiler.joos1w.ast.PackageDeclaration
import compiler.joos1w.ast._

final case class QualifiedNameCollision(
    private val message: String = "Name collision",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Root {
  val ROOT_PKG_NAME = ""
}

class Root() extends Env {
  private val emptyAST = new Empty()
  private val EmptyPackage = new Package(this, Right(emptyAST))

  type Namespace = Map[Name, Env]

  var namespace: Namespace = Map(
    PackageName.ROOT -> EmptyPackage,
    Name("int") -> new Primitive("int"),
    Name("boolean") -> new Primitive("boolean"),
    Name("char") -> new Primitive("char"),
    Name("byte") -> new Primitive("byte"),
  )

  // Attempts to interpret a general qualified name as a package name, class name or interface name
  def getItem(name: QualifiedName): Option[Env] = {
    namespace.get(name.toClassName) match {
      case None =>
        namespace.get(name.toInterfaceName) match {
          case None =>
            namespace.get(name.toPackageName)
          case Some(item: Env) => Some(item)
        }
      case Some(item: Env) => Some(item)
    }
  }

  def populateNamespace(asts: List[AST]): Root = {
    // Populate all packages with their classes + interfaces
    asts.foreach(ast => {
      val pkg = packageFromAST(Some(ast)).populateNamespace
      addItem(pkg.name, pkg)
      pkg.allItems.foreach(item => {
        addItem(item.name, item)
      })
    })

    // Once the packages, classes and interfaces are defined,
    // populate all classes with their fields, constructors, methods
    namespace.values.foreach({
      case pkg: Package =>
        pkg.allItems.foreach(item => {
          item.populateNamespace()
        })
      case _ =>
    })

    // Lastly, now that packages are populated with their classes
    // and interfaces; and classes, interfaces populated with their
    // methods and fields, we have all we need to populate variable
    // and block environments
    namespace.values.foreach({
      case pkg: Package =>
        pkg.allItems.foreach(pkgItem => {
          pkgItem.allItems.foreach({
            case clsItem: Method =>
              clsItem.populateNamespace()
            case _ =>
          })
        })
      case _ =>
    })

    this
  }

  def packageFromAST(ast: Option[AST]): Package = {
    val pkgs: List[Package] =
      AST.visit(
        (ast: Option[AST],
         acrossRec: List[Package] => List[Package],
         downRec: List[Package] => List[Package]) => {
          ast match {
            case Some(pkgAST: PackageDeclaration) =>
              List(new Package(this, Left(pkgAST)))
            case Some(emptyAST: Empty) =>
              if (emptyAST.fieldName == "package_declaration")
                List(new Package(this, Right(emptyAST)))
              else Nil
            case None => Nil
            case _    => downRec(Nil) ++ acrossRec(Nil)
          }
        },
        ast,
        List()
      )
    pkgs.head
  }

  def hasPackage(name: QualifiedName): Boolean = {
    namespace contains name.toPackageName
  }

  def hasClass(name: QualifiedName): Boolean = {
    namespace contains name.toClassName
  }

  def hasInterface(name: QualifiedName): Boolean = {
    namespace contains name.toInterfaceName
  }

  def getPackage(name: PackageName): Option[Package] = {
    getItem(name) match {
      case Some(pkg: Package) => Some(pkg)
      case _                  => None
    }
  }

  def getClass(name: ClassName): Option[Class] = {
    getItem(name) match {
      case Some(cls: Class) => Some(cls)
      case _                => None
    }
  }

  def getInterface(name: InterfaceName): Option[Interface] = {
    getItem(name) match {
      case Some(int: Interface) => Some(int)
      case _                    => None
    }
  }

  def allClasses: List[Env] = {
    namespace
      .filter {
        case (_, item) =>
          item match {
            case _: Class => true
            case _        => false
          }
      }
      .values
      .toList
  }

  def addPackage(name: PackageName, pkg: Package): Root = {
    if (hasClass(name.toQualifiedName)) {
      throw QualifiedNameCollision(
        s"Class exists with same qualified name as package $name")
    }
    if (hasInterface(name.toQualifiedName)) {
      throw QualifiedNameCollision(
        s"Interface exists with same qualified name as package $name")
    }
    getItem(name) match {
      case Some(existingPkg: Package) => // existing package, merge it
        val mergedPkg = existingPkg + pkg
        namespace = namespace + (name -> mergedPkg)
      case None => // new package
        namespace = namespace + (name -> pkg)
        // insert empty packages for parents
        pkg.name.parentPackageNames.foreach(name => {
          val placeholderPkg = new PackageDeclaration()
          placeholderPkg.addChildToEnd(new compiler.joos1w.ast.Name(name.name))
          addPackage(name, new Package(this, Left(placeholderPkg)))
        })
      case _ => throw new RuntimeException("should never happen")
    }
    this
  }

  def hasItem(name: QualifiedName): Boolean = {
    hasClass(name) || hasInterface(name) || hasPackage(name)
  }

  def addClass(name: ClassName, cls: Class): Root = {
    if (hasItem(name.toQualifiedName)) {
      throw QualifiedNameCollision(
        s"Item exists with same name as class $name $toStrTree")
    }
    namespace = namespace + (name -> cls)
    this
  }

  def addInterface(name: InterfaceName, int: Interface): Root = {
    if (hasItem(name.toQualifiedName)) {
      throw QualifiedNameCollision(
        s"Item exists with same name as class $name $toStrTree")
    }
    namespace = namespace + (name -> int)
    this
  }

  def addItem(name: QualifiedName, item: Env): Root = {
    (name, item) match {
      case (clsName: ClassName, cls: Class) =>
        addClass(clsName, cls)
      case (intName: InterfaceName, int: Interface) =>
        addInterface(intName, int)
      case (pkgName: PackageName, pkg: Package) =>
        addPackage(pkgName, pkg)
      case _ => throw new RuntimeException("should not happen")
    }
  }

  override def toStrTree: String = {
    val cs: List[String] = namespace.toList
      .map({
        case (name: Name, env: Env) =>
          val childStrs = env.toStrTree.split("\n")
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
    val (nPkg, nCls, nInt, nPrim) =
      namespace.keys.foldLeft((0, 0, 0, 0))({
        case ((npkg, ncls, nint, nprim), name) =>
          name match {
            case _: PackageName   => (npkg + 1, ncls, nint, nprim)
            case _: ClassName     => (npkg, ncls + 1, nint, nprim)
            case _: InterfaceName => (npkg, ncls, nint + 1, nprim)
            case _: Name          => (npkg, ncls, nint, nprim + 1)
          }
      })
    s"Environment(npackages: $nPkg, nclasses: $nCls, ninterfaces: $nInt)"
  }

  override def lookup(name: Name): Option[Env] = {
    namespace.get(name)
  }

  override def globalLookup(name: Name): Option[Env] = {
    namespace.get(name)
  }
}
