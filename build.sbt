name := "z80_sim"
version := "0.5"
scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
    "jline" % "jline" % "2.14.6",
    "io.github.krzys9876" %% "command-line-reader" % "1.0.0",
    "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")

// set main class
assembly / mainClass := Some("org.kr.scala.z80.Main")
// add fat jar to list of artifacts (sbt-assembly plugin)
assembly / artifact := (assembly / artifact).value.withClassifier(Some("assembly"))
addArtifact(assembly / artifact, assembly)
