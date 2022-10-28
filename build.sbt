name := "volga"

val publishVersion = "0.1"

val oldSettings = Vector(
  libraryDependencies ++= List(
    "org.typelevel"              %% "cats-core"         % "2.0.0",
    "com.github.julien-truffaut" %% "monocle-macro"     % "2.0.0",
    "ru.tinkoff"                 %% "tofu-optics-macro" % "0.5.5",
    "ru.tinkoff"                 %% "tofu-core"         % "0.5.5",
    "org.typelevel"              %% "simulacrum"        % "1.0.0",
  ),
  libraryDependencies ++= List(
    "com.lihaoyi"       %% "fastparse"       % "2.1.3",
    "org.scalatest"     %% "scalatest"       % "3.1.0",
    "org.scalacheck"    %% "scalacheck"      % "1.14.1",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.1.0.0",
    "org.typelevel"     %% "mouse"           % "0.23",
  ) map (_ % Test),
  libraryDependencies ++=
    List(
      compilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.patch),
      compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
    ),
  scalaVersion := "2.13.9",
  crossScalaVersions := List("2.13.9"),
  scalacOptions ++=
    List(
      "-language:higherKinds",
      "-language:postfixOps",
      "-deprecation",
      "-Ymacro-annotations",
    )
)

val oldMacroDeps = List(
  libraryDependencies ++= List(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  ),
  scalacOptions += "-language:experimental.macros"
)

lazy val oldCore = (project in file("modules/old/core")).settings(oldSettings)
lazy val oldMacros =
  (project in file("modules/old/macros")).settings(oldSettings ++ oldMacroDeps).dependsOn(oldCore)

lazy val volga =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(oldCore, oldMacros)

lazy val publishSettingsOld = List(
  moduleName := { "volga-" + name.value },
  publishMavenStyle := true,
  homepage := Some(url("https://manatki.org/docs/derevo")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/manatki/derevo"),
      "git@github.com:manatki/derevo.git"
    )
  ),
  publishTo := {
    if (isSnapshot.value) {
      Some(Opts.resolver.sonatypeSnapshots)
    } else sonatypePublishToBundle.value
  },
  developers := List(
    Developer(
      "odomontois",
      "Oleg Nizhnik",
      "odomontois@gmail.com",
      url("https://github.com/odomontois")
    )
  ),
  organization := "org.manatki",
  version := {
    val branch = git.gitCurrentBranch.value
    if (branch == "master") publishVersion
    else s"$publishVersion-$branch-SNAPSHOT"
  },
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)
