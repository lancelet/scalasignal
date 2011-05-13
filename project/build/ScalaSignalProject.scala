import sbt._

class ScalaSignalProject(info: ProjectInfo) extends DefaultProject(info) {

  val scalaTest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1"
  // val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8"

}
