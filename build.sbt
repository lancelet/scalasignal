name := "scalasignal"

version := "0.4-SNAPSHOT"

organization := "com.github.scalasignal"

scalaVersion := "2.10.0"

// Maven repositories
resolvers ++= Seq(
  "Sonatype Snapshots" at 
    "https://oss.sonatype.org/content/repositories/snapshots/"
)

// Runtime library dependencies
libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "net.sourceforge.jtransforms" % "jtransforms" % "2.4.0"
)

// Test library dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

// Scalac Options
scalacOptions ++= Seq(
  "-Yno-adapted-args", 
  "-Ywarn-all", 
  "-Xfatal-warnings",
  "-deprecation",
  "-feature"
)
