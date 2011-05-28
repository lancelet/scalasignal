import sbt._
import de.element34.sbteclipsify._

class ScalaSignalProject(info: ProjectInfo) extends DefaultProject(info) 
with Eclipsify {

  val scalaToolsSnapshots = "Scala-Tools Snapshots" at 
    "http://scala-tools.org/repo-snapshots"

  // ScalaLA stuff
  val scalaNLPRepo = "ScalaNLP" at "http://repo.scalanlp.org/repo"
  val ondexRepo = "ondex" at 
    "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalala = "org.scalala" %%  "scalala" % "1.0.0.RC2-SNAPSHOT"

  val scalaTest = "org.scalatest" %% "scalatest" % "1.4.1"
  // val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8"

  // Apache Commons Math
  //val commonsMath = "org.apache.commons" % "commons-math" % "2.0"

  // JTransforms
  val signalmlRepo = "SignalMLRepo" at "http://signalml.org/maven/repository"
  val jTransforms = "edu.emory.mathcs" % "jtransforms" % "2.3"
  
  // enable unchecked warnings
  //override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}
