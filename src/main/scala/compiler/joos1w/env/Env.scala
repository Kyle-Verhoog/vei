package compiler.joos1w.env

abstract class Env() {
  def lookup(name: Name): Option[Env]

  // val name: Name

  // namespace???
  // def populateNamespace()
  // getItem
  // hasItem

  def globalLookup(name: Name): Option[Env]

  def toStrTree: String
}
