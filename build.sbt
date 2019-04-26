name := "volga"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

lazy val core = project in file("modules/core")
lazy val macros = project in file("modules/macros")

scalacOptions ++=
  List(
    "-Ypartial-unification",
    "-language:higherKinds"
  )