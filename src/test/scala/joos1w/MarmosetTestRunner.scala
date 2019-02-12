package joos1w

import compiler.Joos1WCompiler
import org.scalatest.FunSuite

import scala.io.Source

class MarmosetTestRunner extends FunSuite {
  test("a1") {
    val files = TestUtils.marmosetTestFiles("a1")
    var i = 0

    for (file <- files) {
      i += 1
      println(s"on test $i out of ${files.length}")
      println(file)
      val filePath = s"src/main/resources/test/marmoset/a1/${file}.java"
      val fileContents = Source.fromFile(filePath).mkString
      // val fileLines = fileContents.split("\n")
      // val JoosLine = if (fileLines.nonEmpty) fileLines(0) else ""
      val JoosLine = fileContents

      if (JoosLine.contains("_EXCEPTION") || JoosLine.contains("INVALID")) {
        assertThrows[Exception](Joos1WCompiler.compileFile(filePath))
      } else {
        Joos1WCompiler.compileFile(filePath)
      }
    }
  }

  test("a2") {
    val files = TestUtils.marmosetTestFiles("a2")
    var i = 0

    for (file <- files) {
      i += 1
      println("on test " + i + " out of " + files.length)
      println(file)
      val filePath = "src/main/resources/test/marmoset/a2/" + file + ".java"
      val fileContents = Source.fromFile(filePath).mkString

      if (fileContents.contains("_EXCEPTION") || fileContents.contains(
            "INVALID")) {
        assertThrows[Exception](Joos1WCompiler.compileFile(filePath))
      } else {
        Joos1WCompiler.compileFile(filePath)
      }
    }
  }
}
