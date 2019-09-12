name := "gatto2"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

scalafmtConfig in ThisBuild := Some(file("scalafmt.conf"))