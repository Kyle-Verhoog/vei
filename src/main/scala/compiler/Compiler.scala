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
        case files=> optionMap(map ++ Map('files -> files.mkString(" ")), Nil)
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

    if (options contains 'files) {
      try {
        Joos1WCompiler.compileFiles(options('files).asInstanceOf[String].split(" ").toList)
      } catch {
        case e: Exception =>
          println(e)
          System.exit(42)
      }
      System.exit(0)
    } else {
      System.exit(0)
    }
  }
}
