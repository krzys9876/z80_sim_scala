name := "z80_sim"
version := "0.3"
scalaVersion := "2.13.10"

val coreDependencies = Seq("jline" % "jline" % "2.14.6")
val testDependencies = Seq("org.scalatest" %% "scalatest" % "3.2.15" % Test)
libraryDependencies ++= coreDependencies ++ testDependencies

ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")

// set main class
assembly / mainClass := Some("org.kr.scala.z80.Main")
// add fat jar to list of artifacts (sbt-assembly plugin)
assembly / artifact := (assembly / artifact).value.withClassifier(Some("assembly"))
addArtifact(assembly / artifact, assembly)
