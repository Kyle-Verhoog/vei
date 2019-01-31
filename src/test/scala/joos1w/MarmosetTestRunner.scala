package joos1w

import compiler.Compiler.runTestFile
import org.scalatest.FunSuite

import scala.io.Source

class MarmosetTestRunner extends FunSuite {
  test("a1") {
    val files = TestUtils.marmosetTestFiles("a1")
    var i = 0

    for (file <- files) {
      i += 1
      println("on test " + i + " out of " + files.length)
      println(file)
      val filePath = "test/marmoset/a1/" + file + ".java"
      val fileContents = Source.fromResource(filePath).mkString

      if (fileContents.contains("_EXCEPTION") || fileContents.contains(
            "INVALID")) {
        assertThrows[Exception](runTestFile(filePath))
      } else {
        runTestFile(filePath)
      }
    }
  }

  test("a2") {
    val files = Joos1WTestUtils.marmosetTestFiles("a2")
    var i = 0

    for (file <- files) {
      i += 1
      println("on test " + i + " out of " + files.length)
      println(file)
      val filePath = "test/marmoset/a2/" + file + ".java"
      val fileContents = Source.fromResource(filePath).mkString

      if (fileContents.contains("_EXCEPTION") || fileContents.contains(
        "INVALID")) {
        assertThrows[Exception](runTestFile(filePath))
      } else {
        runTestFile(filePath)
      }
    }
  }
}
