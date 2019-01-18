import scala.io.Source

object Joos1WTestUtils {
  def featureTestFiles(): List[String] = {
    Source
      .fromResource("test/features/")
      .mkString
      .split('\n')
      .map(s => s.split('.')(0))
      .distinct
      .toList
  }
}
