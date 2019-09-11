name := "gatto2"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
