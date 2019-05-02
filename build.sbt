name := "volga"

version := "0.1"

scalaVersion in ThisBuild := "2.12.8"

val libs = libraryDependencies ++= List(
  "org.typelevel" %% "cats-core" % "1.6.0",
  "com.github.julien-truffaut" %%  "monocle-macro" % "1.5.1-cats"
)

val plugins = libraryDependencies ++=
  List(
    compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0")
  )

val macroDeps = List(
  libraryDependencies ++= List(
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  ),
  scalacOptions += "-language:experimental.macros"
)

lazy val core   = (project in file("modules/core")).settings(plugins, libs)
lazy val macros = (project in file("modules/macros")).settings(macroDeps).dependsOn(core)

scalacOptions ++=
  List(
    "-Ypartial-unification",
    "-language:higherKinds"
  )
