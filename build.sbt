name := "z80_sim"
ThisBuild / version := "0.0.8"
ThisBuild / versionScheme := Some("early-semver")

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
    "jline" % "jline" % "2.14.6",
    "io.github.krzys9876" %% "command-line-reader" % "1.1.0",
    "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")

// set main class
assembly / mainClass := Some("org.kr.scala.z80.Main")
// add fat jar to list of artifacts (sbt-assembly plugin)
assembly / artifact := (assembly / artifact).value.withClassifier(Some("assembly"))
addArtifact(assembly / artifact, assembly)

overridePublishSettings

ThisBuild / organization := "io.github.krzys9876"
ThisBuild / organizationName := "krzys9876"
ThisBuild / organizationHomepage := Some(url("https://github.com/krzys9876"))
ThisBuild / scmInfo := Some(
    ScmInfo(
        url("https://github.com/krzys9876/z80_sim_scala"),
        "scm:git@github.com:krzys9876/z80_sim_scala.git"))
ThisBuild / developers := List(
    Developer(
        id = "krzys9876",
        name = "Krzysztof Ruta",
        email = "krzys9876@gmail.com",
        url = url("https://github.com/krzys9876")))
ThisBuild / description := "Simulate Z80 processor and basic peripherals: memory and IO ports."
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/license/mit/"))
ThisBuild / homepage := Some(url("https://github.com/krzys9876/z80_sim_scala"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
    val nexus = "https://s01.oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true
