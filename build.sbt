name := "scalasignal"

version := "0.3-SNAPSHOT"

organization := "com.github.scalasignal"

scalaVersion := "2.9.1"

// Maven repositories
resolvers ++= Seq(
  "Scala-Tools Snapshots" at "http://scala-tools.org/repo-snapshots",
  "OnDex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public",
  "ScalaNLP" at "http://repo.scalanlp.org/repo",
  "SignalML" at "http://signalml.org/maven/repository"
)

// Runtime library dependencies
libraryDependencies ++= Seq(
  "org.scalala" % "scalala_2.9.0" % "1.0.0.RC2-SNAPSHOT",
  "edu.emory.mathcs" % "jtransforms" % "2.3"
)

// Test library dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"
