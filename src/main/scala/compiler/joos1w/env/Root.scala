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

class Root(val asts: List[AST]) extends Env {
  private val emptyAST = new Empty()
  private val EmptyPackage = new Package(this, Right(emptyAST))

  type Namespace = Map[Name, Option[Env]]

  var namespace: Namespace = Map(
    PackageName.ROOT -> Some(EmptyPackage)
  )

  def hasPackage(name: Name): Boolean = {
    namespace.contains(name)
  }

  def hasItem(name: Name): Boolean = {
    namespace.contains(name)
  }

  def getItem(name: Name): Option[Env] = {
    if (hasItem(name)) {
      namespace(name) match {
        case Some(cls: Class) =>
          name match {
            case _: ClassName => Some(cls)
            case _ =>
              throw new RuntimeException(
                s"Got class $cls for non-classname $name")
          }
        case Some(pkg: Package) =>
          name match {
            case _: PackageName => Some(pkg)
            case _ =>
              throw new RuntimeException(
                s"Got pkg $pkg for non-classname $name")
          }
        case _ =>
          throw new RuntimeException(s"Got unexpected name for getItem $name")
      }
    } else {
      None
    }
  }

  def addPackagesFromASTs(asts: List[AST] = asts): Unit = {
    asts.foreach(ast => {
      val pkg = packageFromAST(Some(ast))
      addPackage(pkg.name, Some(pkg))
      pkg.getAllClasses.foreach(cls => {
        addClass(cls.name, Some(cls))
      })
    })
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

  def addClass(name: ClassName, cls: Option[Class]): Unit = {
    if (hasItem(name)) {
      throw QualifiedNameCollision(s"Duplicate class $name")
    }
    namespace = namespace + (name -> cls)
  }

  def addNewPackage(name: PackageName, pkg: Option[Package]): Unit = {
    pkg match {
      case Some(newPkg) =>
        namespace = namespace + (name -> Some(newPkg))
        newPkg.name.parentPackageNames.foreach(name => {
          addEmptyPackageIfDNE(name)
        })
      case None =>
        namespace = namespace + (name -> None)
    }
  }

  def addEmptyPackageIfDNE(name: PackageName): Unit = {
    if (!hasPackage(name)) {
      addNewPackage(name, None)
    }
  }

  def addPackage(name: PackageName, pkg: Option[Package]): Unit = {
    if (hasPackage(name)) {
      getPackage(name) match {
        case Some(curPkg) =>
          pkg match {
            case Some(newPkg) =>
              val mergePkg = curPkg + newPkg
              namespace = namespace + (mergePkg.name -> Some(mergePkg))
            case None =>
          }
        case None =>
          addNewPackage(name, pkg)
      }
    } else {
      addNewPackage(name, pkg)
    }
  }

  def getPackage(name: Name): Option[Package] = {
    getItem(name) match {
      case Some(pkg: Package) => Some(pkg)
      case None               => None
    }
  }

  def getClass(name: ClassName): Option[Class] = {
    getItem(name) match {
      case Some(cls: Class) => Some(cls)
      case None             => None
    }
  }

  def getAllClasses: List[Option[Env]] = {
    namespace
      .filter {
        case (_, item) =>
          item match {
            case Some(_: Class) => true
            case _              => false
          }
      }
      .values
      .toList
  }

  override def toStrTree: String = {
    val cs: List[String] = namespace.toList
      .map({
        case (name: Name, item: Option[Env]) =>
          item match {
            case Some(env: Env) =>
              val childStrs = env.toStrTree.split("\n")
              val tailChar = if (childStrs.tail.isEmpty) "" else "\n"
              s"┠─ " + childStrs.head + tailChar + childStrs.tail
                .map(
                  line => "┃  " + line
                )
                .mkString("\n")
            case None => s"$name: None"
            case _    => ""
          }
      })
    val scs = cs.mkString("\n")
    s"$toString\n$cs"
  }

  override def toString: String = {
    val (nPackages, nClasses) = namespace.keys.foldLeft[(Int, Int)]((0, 0))({
      case ((npkg, ncls), name) =>
        name match {
          case _: PackageName => (npkg + 1, ncls)
          case _: ClassName   => (npkg, ncls + 1)
        }
    })
    s"Environment(npackages: $nPackages, nclasses: $nClasses)"
  }

  override def lookup(qualifiedName: Name): Option[Env] = {
    None
  }

  override def globalLookup(qualifiedName: Name): Option[Env] = {
    None
  }
}
