package regex
import scala.collection.mutable.ListBuffer

case class Paren(nalt: Integer, natom: Integer) {
  def getnalt(): Integer = {
    nalt
  }
  def getnatom(): Integer = {
    natom
  }
}

object Regex {
  def toPostfix(regex: String): String = {
    var postfix = ""
    var cch = '@'
    var x = 0
    var natom = 0
    var nalt = 0
    var parens = new ListBuffer[Paren]()

    for (x <- 0 until regex.length()) {
      var re = regex.charAt(x)

      re match {
        case '(' => {
          if (natom > 1) {
            natom -= 1
            postfix += cch
          }

          parens += new Paren(nalt, natom)
          nalt = 0
          natom = 0
        }
        case '|' => {
          if (natom == 0) {
            println("Error") // TODO: throw
          }
          natom -= 1
          while (natom > 0) {
            postfix += cch
            natom -= 1
          }
          nalt += 1
        }
        case ')' => {
          if (parens.length < 1) {
            println("error 1") // TODO: throw
          }
          if (natom == 0) {
            println("error 2") // TODO: throw
          }

          natom -= 1
          while (natom > 0) {
            postfix += cch
            natom -= 1
          }

          while (nalt > 0) {
            postfix += '|'
            nalt -= 1
          }

          var par = parens.remove(parens.length - 1)
          nalt = par.getnalt()
          natom = par.getnatom()
          natom += 1
        }
        case '?' | '*' | '+' => {
          if (natom == 0) {
            println("Error symb") // throws
          }
          postfix += re
        }
        case _ => {
          if (natom > 1) {
            natom -= 1
            postfix += cch
          }

          postfix += re
          natom += 1
        }
      }
    }

    if (parens.length > 0) {
      // TODO throw
    }

    natom -= 1
    while (natom > 0) {
      postfix += cch
      natom -= 1
    }

    while (nalt > 0) {
      postfix += '|'
      nalt -= 1
    }
    postfix
  }

  def toNFA(regex: String) {}
}
