name := "scalasignal"

version := "0.3-SNAPSHOT"

organization := "com.github.scalasignal"

scalaVersion := "2.9.1-1"

// Maven repositories
resolvers ++= Seq(
  "Sonatype" at "https://oss.sonatype.org/content/groups/public"
)

// Runtime library dependencies
libraryDependencies ++= Seq(
  "org.scalala" % "scalala_2.9.1" % "1.0.0.RC2"
)

// Test library dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"
