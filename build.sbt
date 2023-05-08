name := "volga"

val publishVersion = "0.2"

val scala3version = "3.3.0-RC5"
val scala2version = "2.13.10"

crossScalaVersions := List(scala3version, scala2version)

publish / skip := true

val oldMacroDeps = List(
  libraryDependencies ++= List(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  ),
  scalacOptions += "-language:experimental.macros"
)

val modules = file("modules")

val publishSettings = List(
  moduleName        := { "volga-" + name.value },
  publishMavenStyle := true,
  homepage          := Some(url("https://manatki.org/docs/derevo")),
  scmInfo           := Some(
    ScmInfo(
      url("https://github.com/manatki/derevo"),
      "git@github.com:manatki/derevo.git"
    )
  ),
  developers        := List(
    Developer(
      "odomontois",
      "Oleg Nizhnik",
      "odomontois@gmail.com",
      url("https://github.com/odomontois")
    )
  ),
  organization      := "org.manatki",
  version           := {
      val branch = git.gitCurrentBranch.value
      if (branch == "master") publishVersion
      else s"$publishVersion-$branch-SNAPSHOT"
  },
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

val commonSettings = Vector(
  scalaVersion       := scala3version,
  crossScalaVersions := List(scala3version),
  scalacOptions ++= Vector(
    // "-source",
    // "future",
    "-Ykind-projector:underscores",
    "-Yshow-suppressed-errors",
    "-Yexplicit-nulls"
  )
)

lazy val core = project
    .in(modules / "core")
    .settings(
      commonSettings,
      libraryDependencies += "net.sourceforge.plantuml" % "plantuml" % "1.2022.14" % Test,
      libraryDependencies += "org.scalameta"           %% "munit"    % "1.0.0-M7"    % Test
    )

lazy val prob = project.in(modules / "prob").settings(commonSettings).dependsOn(core)

val oldSettings = Vector(
  libraryDependencies ++= List(
    "org.typelevel" %% "cats-core"     % "2.8.0",
    "dev.optics"    %% "monocle-macro" % "3.1.0",
    "tf.tofu"       %% "glass-core"    % "0.2.1",
    "tf.tofu"       %% "glass-macro"   % "0.2.1",
    "org.typelevel" %% "simulacrum"    % "1.0.1"
  ),
  libraryDependencies ++= List(
    "com.lihaoyi"       %% "fastparse"       % "2.2.2",
    "org.scalatest"     %% "scalatest"       % "3.2.14",
    "org.scalacheck"    %% "scalacheck"      % "1.15.4",
    "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0",
    "org.typelevel"     %% "mouse"           % "1.2.0"
  ) map (_               % Test),
  libraryDependencies ++=
      List(
        compilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.patch),
        compilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")
      ),
  scalaVersion       := scala2version,
  crossScalaVersions := List(scala2version),
  scalacOptions ++=
      List(
        "-language:higherKinds",
        "-language:postfixOps",
        "-deprecation",
        "-Ymacro-annotations"
      ),
  publish / skip     := true
)

lazy val old = modules / "old"

lazy val oldCore = project.in(old / "core").settings(oldSettings)

lazy val oldMacros = project.in(old / "macros").settings(oldSettings ++ oldMacroDeps).dependsOn(oldCore)
