name := "gatto2"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= List(
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
  "org.typelevel" %% "discipline-scalatest" % "1.0.0-M1"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

scalafmtConfig in ThisBuild := Some(file("scalafmt.conf"))