package compiler.joos1w.environment
import compiler.joos1w.ast.AST

import scala.collection.mutable

class RootEnvironment(val myAst: AST, val parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  val packageEnvironments: mutable.HashMap[String, PackageEnvironment] =
    mutable.HashMap[String, PackageEnvironment]()

  override def findPackageEnv(name: String): Option[PackageEnvironment] = {
    packageEnvironments.get(name)
  }

  override def createOrReturnRootPackageEnv(
      name: String): PackageEnvironment = {
    if (packageEnvironments.contains(name)) return packageEnvironments(name)
    // TODO values that arent null?
    println("creating new pkg env for: " + name)
    packageEnvironments += name -> new PackageEnvironment(null, Option(this))
    insertChild(packageEnvironments(name))

    packageEnvironments(name)
  }

  override def retrieveAllClassesInPackage(name: String): Map[String, AST] = {
    print("searching for class in pkg " + name)
    val classes = mutable.Map[String, AST]()

    if (name.split('.').last == "*") {
      // get all classes from packages that have this as a prefix
      packageEnvironments.keySet
        .filter(key => key.startsWith(name.take(name.length - 2)))
        .map(pkgName => retrieveAllClassesInPackage(pkgName))
        .foreach(mapping => {
          mapping.keys.foreach(className => {
            if (classes.contains(className))
              throw new RuntimeException(
                "Inner package has class that was already defined with name: " + className)
            classes += className -> mapping(className)
          })
        })
      classes.toMap
    } else {
      if (findPackageEnv(name).isEmpty)
        throw new RuntimeException("Package not found: " + name)
      findPackageEnv(name).get.classTable.toMap
    }
  }
}
