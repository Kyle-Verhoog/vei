package compiler.joos1w.env

import compiler.joos1w.ast._

import scala.collection.mutable

object Root {
  val ROOT_PKG_NAME = ""
}

class Root(val asts: List[AST]) extends Env {
  type PackageMap = Map[String, Option[Package]]
  var packages: PackageMap = Map()

  def hasPackage(name: String): Boolean = {
    packages.contains(name)
  }

  def addPackagesFromASTs(asts: List[AST] = asts): Unit = {
    asts.foreach(ast => {
      val pkg = packageFromAST(Some(ast))
      addPackage(pkg.name, Some(pkg))
    })
  }

  def packageFromAST(ast: Option[AST]): Package = {
    AST
      .foldDown[PackageDeclaration, List[Package]](
        (ast, acc) => new Package(this, ast) :: acc,
        ast,
        List(),
        AST.RecursionOptions(true, true, true, (r1, r2) => r1 ++ r2)
      )
      .head
  }

  def addNewPackage(name: String, pkg: Option[Package]): Unit = {
    pkg match {
      case Some(newPkg) =>
        packages = packages + (name -> Some(newPkg))
        newPkg.parentNames.foreach(name => {
          println(name)
          addEmptyPackageIfDNE(name)
        })
      case None =>
        packages = packages + (name -> None)
    }
  }

  def addEmptyPackageIfDNE(name: String): Unit = {
    if (!hasPackage(name)) {
      addNewPackage(name, None)
    }
  }

  def addPackage(name: String, pkg: Option[Package]): Unit = {
    if (hasPackage(name)) {
      getPackage(name) match {
        case Some(existingPkg) =>
          throw new RuntimeException(
            s"Overwriting existing pkg on name '$name', existing: $existingPkg")
        case None =>
          addNewPackage(name, pkg)
      }
    } else {
      addNewPackage(name, pkg)
    }
  }

  def getPackage(name: String): Option[Package] = {
    if (hasPackage(name)) packages(name) else None
  }

  def getClass(qualifiedName: String): Option[Class] = {
    val split = qualifiedName.split("\\.")
    val pkg = split.slice(0, split.length - 1).mkString(".")
    val cls = split(split.length)

    getPackage(pkg) match {
      case Some(pkg) => pkg.getClass(cls)
      case None      => None
    }
  }

  def getAllClasses: List[Class] = {
    packages
      .foldLeft(Nil: List[Class]) {
        case (acc, (_, pkg)) =>
          pkg match {
            case Some(pkg) => pkg.getAllClasses ++ acc
            case None      => acc
          }
      }
  }

  /*
  type PkgClasses = (List[Package], List[Class])
  def getPackagesAndClassesFromAST(ast: Option[AST]): PkgClasses = {
    ast match {
      case Some(ast) =>
        ast match {
          case ast: PackageDeclaration =>
            val (pkgs, classes) = getPackagesAndClassesFromAST(ast.rightSibling)
            (new Package(ast.name) :: pkgs, classes)
          case ast: ClassDeclaration =>
            val (pkgs: List[Package], classes) = getPackagesAndClassesFromAST(ast.rightSibling)
            val parent = pkgs match {
              case pkg :: Nil => pkg
              case Nil => defaultPkg
              case _ => defaultPkg // NOTE: not technically possible w/ grammar
            }
            (pkgs, new Class(ast.identifier, Some(parent)) :: classes)
          case _ =>
            val (rpkgs, rclasses) = getPackagesAndClassesFromAST(ast.rightSibling)
            val nodes = ast.rightSibling :: ast.children.map(c => Option(c))
            val results = nodes.foldLeft[PkgClasses]((Nil, Nil)) {
              (prev: PkgClasses, ast: Option[AST]) =>
                val (pkgs, classes) = getPackagesAndClassesFromAST(ast)
                (pkgs ::: prev._1, classes ::: prev._2): PkgClasses
            }
            results
        }
      case None => (Nil, Nil)
    }
  }*/

  override def globalLookup(qualifiedName: _root_.scala.Predef.String)
    : _root_.scala.Option[_root_.compiler.joos1w.env.Env] = {
    None
  }

  //override def globalLookup[T](qualifiedName: String): Option[Env] = {
  //  T match {
  //    case Package => getPackage(qualifiedName)
  //    case Class => getClass(qualifiedName)
  //    case None => None
  //  }
  //  None
  //}
}
