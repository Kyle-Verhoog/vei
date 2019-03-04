package compiler.joos1w.env

import compiler.joos1w.ast._

object Root {
  def makeRoot(ast: AST): Root = {
    new Root(Map[String, Package]()).addAST(Some(ast))
  }
}

class Root(val packages: Map[String, Package],
           val defaultPkg: Package = new Package("", Map())) extends Env {
  def hasPackage(name: String): Boolean = {
    packages.contains(name)
  }

  def addPackage(pkg: Package): Root = {
    // If a package already exists, then combine it with the new one
    // NOTE: this should only happen if a subpackage was added before a parent
    // eg. A.B.C was added and then A added, 'A' would be overwritten
    val newPkg = if (packages.contains(pkg.name)) {
      val existingPkg = packages(pkg.name)
      if (existingPkg.numClasses > 0) {
        throw new RuntimeException("Adding package for existing")
      }
      existingPkg + pkg
    }
    else {
      pkg
    }
    val newPackages = packages + (newPkg.name -> newPkg)
    new Root(
      newPackages,
      defaultPkg,
    )
  }

  def getPackage(name: String): Package = {
    packages(name)
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

  def +(other: Root): Root = {
    new Root(
      packages ++ other.packages,
      defaultPkg + other.defaultPkg,
    )
  }

  def addAST(ast: Option[AST]): Root = {
    ast match {
      case Some(ast) =>
        ast match {
          case ast: PackageDeclaration =>
            val root = addAST(ast.rightSibling)
            val pkg = Package.fromAST(ast)
            root.addPackage(pkg)
          case _ => // just recursively merge all results we get
            val nodes = ast.rightSibling :: ast.children.map(c => Option(c))
            val results = nodes.foldLeft[Root](new Root(Map())) {
              (prev, ast) =>
                val root = addAST(ast)
                prev + root
            }
            results
        }
      case None => this
    }
  }

  override def lookup(name: String): Option[Env] = {
    None
  }
}
