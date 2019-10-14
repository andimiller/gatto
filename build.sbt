name := "gatto2"

version := "0.1"

lazy val commonSettings = List(
  scalaVersion := "2.12.10",
  scalacOptions += "-Ypartial-unification",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  scalafmtConfig in ThisBuild := Some(file("scalafmt.conf"))
)

lazy val core = project.in(file(".")).settings(commonSettings).settings(
  libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

lazy val bench = project.in(file("bench")).dependsOn(core).settings(commonSettings).settings(
  libraryDependencies += "org.tpolecat" %% "atto-core" % "0.7.1",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.3"
).enablePlugins(JmhPlugin)