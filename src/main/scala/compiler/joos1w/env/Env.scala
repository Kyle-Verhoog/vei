package compiler.joos1w.env

abstract class Env() {
  def lookup(name: Name): Option[Env]

  def globalLookup(name: Name): Option[Env]
}
