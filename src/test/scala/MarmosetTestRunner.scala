import compiler.Compiler.runTestFile
import org.scalatest.FunSuite

import scala.io.Source

class MarmosetTestRunner extends FunSuite {
  test("a1") {
    val files = Joos1WTestUtils.marmosetTestFiles("a1").takeRight(248)
    var i = 0

    for (file <- files) {
      i += 1
      println("on test " + i + " out of " + files.length)
      val filePath = "test/marmoset/a1/" + file + ".java"

      if (Source.fromResource(filePath).mkString.contains("_EXCEPTION")) {
        assertThrows[Exception](runTestFile(filePath))
      } else {
        runTestFile(filePath)
      }
    }
  }
}
