package joos1w

import compiler.joos1w.Joos1WCompiler
import org.scalatest.FunSuite

import scala.io.Source

class FeaturesTestRunner extends FunSuite {
  test("Features") {
    val files = TestUtils.featureTestFiles()
    var i = 0

    for (file <- files) {
      i += 1
      println(s"on test $i out of ${files.length}")
      println(file)
      val filePath = s"src/main/resources/test/features/${file}.java"
      val fileContents = Source.fromFile(filePath).mkString
      val fileLines = fileContents.split("\n")
      val JoosLine = if (fileLines.nonEmpty) fileLines(0) else ""

      if (JoosLine.contains("FAIL") || JoosLine.contains("INVALID")) {
        assertThrows[Exception](Joos1WCompiler.compileFile(filePath))
      } else {
        Joos1WCompiler.compileFile(filePath)
      }
    }
  }
}
