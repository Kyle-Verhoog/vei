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

  def marmosetTestFiles(assignmentVersion: String): List[String] = {
    Source
      .fromResource("test/marmoset/" + assignmentVersion)
      .mkString
      .split('\n')
      .map(s => s.split('.')(0))
      .distinct
      .toList
  }
}
