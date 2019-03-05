package compiler.joos1w.env


abstract class Env() {
  def globalLookup(qualifiedName: String): Option[Env]

  // def lookup(name: String): Option[Env]
}