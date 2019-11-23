name := "volga"

version := "0.1"

scalaVersion in ThisBuild := "2.13.0"

val libs = libraryDependencies ++= List(
  "org.typelevel" %% "cats-core"         % "2.0.0-RC1" withSources(),
  "com.github.julien-truffaut" %% "monocle-macro" % "2.0.0-RC1" withSources(),
  "ru.tinkoff"    %% "tofu-optics-macro" % "0.5.2" withSources(),
)

val testLibs = libraryDependencies ++= List(
  "com.lihaoyi" %% "fastparse" % "2.1.3"
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

lazy val core    = (project in file("modules/core")).settings(plugins, libs, testLibs)
lazy val macros  = (project in file("modules/macros")).settings(macroDeps, plugins).dependsOn(core)
lazy val rebuild = (project in file("modules/rebuild")).settings(libs, plugins).dependsOn(core, macros)

scalacOptions in ThisBuild ++=
  List(
    "-Ymacro-annotations",
    "-language:higherKinds",
    "-language:postfixOps",
    "-deprecation"
  )
