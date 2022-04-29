name := "z80_sim"

version := "0.1"

scalaVersion := "2.13.8"

val coreDependencies = Seq(
)

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)

libraryDependencies ++= coreDependencies ++ testDependencies
