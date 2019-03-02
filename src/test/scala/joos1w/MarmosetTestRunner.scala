package joos1w

import compiler.joos1w.Joos1WCompiler
import joos1w.TestUtils.getMarmosetLibFiles
import org.scalatest.FunSuite

import scala.io.Source

class MarmosetTestRunner extends FunSuite {
  test("a1") {
    val files = TestUtils.marmosetTestFiles("a1").map(file => file.head)
    var i = 0

    for (file <- files) {
      i += 1
      println(s"on test $i out of ${files.length}")
      println(file)
      val filePath = s"src/main/resources/$file"
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
    val listOfFiles = TestUtils.marmosetTestFiles("a2")
    val expectedResultsFile = s"src/main/resources/test/marmoset/a2_expected"
    val expectedResults = Source
      .fromFile(expectedResultsFile)
      .mkString
      .split("\n")
      .map(l => l.split(" "))
      .toList

    var i = 0
    for (files <- listOfFiles) {
      i += 1
      println("on test " + i + " out of " + listOfFiles.length)
      println(files.mkString(" "))

      val filePaths = files.map(f => "src/main/resources/" + f)

      val libFiles =
        getMarmosetLibFiles("2").map(f => "src/main/resources/" + f)
      expectedResults(i)(1) match {
        case "????" =>
          try {
            Joos1WCompiler.compileFiles(filePaths ++ libFiles)
          } catch {
            case _: Throwable =>
          }
        case "fail" =>
          assertThrows[Exception](
            Joos1WCompiler.compileFiles(filePaths ++ libFiles))
        case "pass" =>
          Joos1WCompiler.compileFiles(filePaths ++ libFiles)
      }
    }
  }
}
