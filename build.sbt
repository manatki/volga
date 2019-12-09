name := "volga"

version := "0.1"

scalaVersion in ThisBuild := "2.13.1"

val libs = libraryDependencies ++= List(
  "org.typelevel"              %% "cats-core"         % "2.0.0",
  "com.github.julien-truffaut" %% "monocle-macro"     % "2.0.0",
  "ru.tinkoff"                 %% "tofu-optics-macro" % "0.5.5",
  "ru.tinkoff"                 %% "tofu-core"         % "0.5.5",
  "org.typelevel"              %% "simulacrum"        % "1.0.0",
)

val testLibs = libraryDependencies ++= List(
  "com.lihaoyi"    %% "fastparse"  % "2.1.3",
  "org.scalatest"  %% "scalatest"  % "3.1.0",
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.0.0",
) map (_ % Test)

val plugins = libraryDependencies ++=
  List(
    compilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )

val macroDeps = List(
  libraryDependencies ++= List(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  ),
  scalacOptions += "-language:experimental.macros"
)

val options = scalacOptions ++=
  List(
    "-Ymacro-annotations",
    "-language:higherKinds",
    "-language:postfixOps",
    "-deprecation"
  )

lazy val core    = (project in file("modules/core")).settings(options, plugins, libs, testLibs)
lazy val macros  = (project in file("modules/macros")).settings(options, macroDeps, plugins, testLibs).dependsOn(core)
lazy val rebuild = (project in file("modules/rebuild")).settings(options, libs, plugins).dependsOn(core, macros)

lazy val volga = project.in(file(".")).aggregate(core, macros, core)
