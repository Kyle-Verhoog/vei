package compiler.joos1w.env

import compiler.joos1w.ast._

/*
class VariableBlock(var parent: Env, val ast: AST) extends Env {

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    // search namespace, if nothing, then parent
    parent.lookup(name)
  }
}*/
