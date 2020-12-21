lazy val commonSettings = Seq(
  name := "chisel-crv",
  organization := "chisel-uvm",
  version := "0.2.5",
  scalaVersion := "2.12.10",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls"),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
  ),
  scalacOptions := Seq("-Xsource:2.11"),

  libraryDependencies ++= Seq(
    "org.jacop" % "jacop" % "4.7.0",
    "org.scalatest" %% "scalatest" % "3.0.8",
    "org.choco-solver" % "choco-solver" % "4.10.5",
    "edu.berkeley.cs" %% "chiseltest" % "0.3-SNAPSHOT"
  ),
  coverageEnabled := true
)

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
lazy val core = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies += scalaReflect.value
)



