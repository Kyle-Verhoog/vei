package compiler

import compiler.joos1w.{Joos1WCompiler, Joos1WParser, Joos1WScanner}

object Compiler {
  val usage = """
      Usage: joosc [--gen-dfa] filename
  """

  def main(args: Array[String]) {
    type OptionMap = Map[Symbol, Any]

    def optionMap(map: OptionMap, list: List[String]): OptionMap = {
      def isSwitch(s: String) = s(0) == '-'
      list match {
        case Nil => map
        case "--gen-scandfa" :: tail =>
          optionMap(map ++ Map('genDfa -> true), tail)
        case "--gen-parsetable" :: tail =>
          optionMap(map ++ Map('genParseTable -> true), tail)
        case string :: opt2 :: _ if isSwitch(opt2) =>
          optionMap(map ++ Map('file -> string), list.tail)
        case string :: Nil => optionMap(map ++ Map('file -> string), list.tail)
        case option :: _ =>
          println(s"Unknown option '$option'")
          System.exit(1)
          Map[Symbol, Any]()
      }
    }

    val options = optionMap(Map(), args.toList)

    if (options contains 'genDfa) {
      Joos1WScanner.generateNewScanner()
      Joos1WScanner.saveScanner("src/main/resources/serializations/dfa")
    }

    if (options contains 'genParseTable) {
      Joos1WParser.generateTableLR1()
    }

    if (options contains 'file) {
      try {
        Joos1WCompiler.compileFile(options('file).asInstanceOf[String])
      } catch {
        case e: Exception =>
          println(e)
          System.exit(42)
      }
      System.exit(0)
    } else {
      Joos1WCompiler.compileFile("src/main/resources/test/Empty.java")
    }
  }
}
