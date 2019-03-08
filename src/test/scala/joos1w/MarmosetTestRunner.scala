package joos1w

import compiler.joos1w.Joos1WCompiler
import joos1w.TestUtils.getMarmosetLibFiles
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
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
      .map(l => l.split(" ").toList)
      .toList
    val getExpectedResult = (files: String) => {
      expectedResults
        .filter(r => files contains r(0))
        .takeWhile(result => true)
        .head
    }

    var i = 0
    var failedTests = ListBuffer[Throwable]()
    for (files <- listOfFiles.drop(0)) {
      i += 1
      val expectedResult = getExpectedResult(files.mkString(" "))
      println(
        "on test " + i + " out of " + listOfFiles.length + " using expected results for " + expectedResult(
          0))
      println(files.mkString(" "))

      val filePaths = files.map(f => "src/main/resources/" + f)

      val libFiles =
        getMarmosetLibFiles("2").map(f => "src/main/resources/" + f)

        expectedResult(1) match {
          case "fail" =>
            assertThrows[Exception](
              Joos1WCompiler.compileFiles(filePaths ++ libFiles))
          case "pass" =>
            Joos1WCompiler.compileFiles(filePaths ++ libFiles)
        }
        println("✓ TEST PASSED")

      println(s"$i/${listOfFiles.length} TESTS RUN")
      println(s"${failedTests.length}/${listOfFiles.length} TESTS FAILED")
    }
    if (failedTests.nonEmpty) {
      throw new RuntimeException()
    }
  }

  test("a3") {
    val listOfFiles = TestUtils.marmosetTestFiles("a3")
    val expectedResultsFile = s"src/main/resources/test/marmoset/a3_expected"
    val expectedResults = Source
      .fromFile(expectedResultsFile)
      .mkString
      .split("\n")
      .map(l => l.split(" ").toList)
      .toList
    val getExpectedResult = (files: String) => {
      expectedResults
        .filter(r => files contains r(0))
        .takeWhile(result => true)
        .head
    }

    var i = 48
    var failedTests = ListBuffer[Throwable]()
    for (files <- listOfFiles.drop(48)) {
      i += 1
      val expectedResult = getExpectedResult(files.mkString(" "))
      println(
        "on test " + i + " out of " + listOfFiles.length + " using expected results for " + expectedResult(
          0))
      println(files.mkString(" "))

      val filePaths = files.map(f => "src/main/resources/" + f)

      val libFiles =
        getMarmosetLibFiles("3").map(f => "src/main/resources/" + f)
      try {
        expectedResult(1) match {
          case "fail" =>
            assertThrows[Exception](
              Joos1WCompiler.compileFiles(filePaths ++ libFiles))
          case "pass" =>
            Joos1WCompiler.compileFiles(filePaths ++ libFiles)
        }
        println("✓ TEST PASSED")
      } catch {
        case e: Throwable =>
          failedTests += e
          println(e)
          println("✗ TEST FAILED")
          throw e
      }
      println(s"$i/${listOfFiles.length} TESTS RUN")
      println(s"${failedTests.length}/${listOfFiles.length} TESTS FAILED")
    }
    if (failedTests.nonEmpty) {
      throw new RuntimeException()
    }
  }
}
