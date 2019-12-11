name := "volga"

val publishVersion = "0.1"

scalaVersion in ThisBuild := "2.13.1"

val libs = libraryDependencies ++= List(
  "org.typelevel"              %% "cats-core"         % "2.0.0",
  "com.github.julien-truffaut" %% "monocle-macro"     % "2.0.0",
  "ru.tinkoff"                 %% "tofu-optics-macro" % "0.5.5",
  "ru.tinkoff"                 %% "tofu-core"         % "0.5.5",
  "org.typelevel"              %% "simulacrum"        % "1.0.0",
)

val testLibs = libraryDependencies ++= List(
  "com.lihaoyi"       %% "fastparse"       % "2.1.3",
  "org.scalatest"     %% "scalatest"       % "3.1.0",
  "org.scalacheck"    %% "scalacheck"      % "1.14.1",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.0.0",
  "org.typelevel"     %% "mouse"           % "0.23",
) map (_ % Test)

val plugins = libraryDependencies ++=
  List(
    compilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.patch),
    compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
  )

val macroDeps = List(
  libraryDependencies ++= List(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  ),
  scalacOptions += "-language:experimental.macros"
)

val options = scalacOptions ++=
  List(
    "-language:higherKinds",
    "-language:postfixOps",
    "-deprecation"
  )

val versionSpecific = List(
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11 | 12)) => List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
      case _                  => List()
    }
  },
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y == 13 => List("-Ymacro-annotations")
      case _                       => List("-Ypartial-unification")
    }
  },
)

val commonSettings = options ++ plugins ++ libs ++ testLibs ++ versionSpecific ++ publishSettings

lazy val core    = (project in file("modules/core")).settings(commonSettings)
lazy val macros  = (project in file("modules/macros")).settings(commonSettings ++ macroDeps).dependsOn(core)
lazy val rebuild = (project in file("modules/rebuild")).settings(commonSettings).dependsOn(core, macros)

lazy val volga =
  project
    .in(file("."))
    .settings(skip in publish := true)
    .aggregate(core, macros)

lazy val publishSettings = List(
  crossScalaVersions := List("2.12.10", "2.13.1"),
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
  credentials ++= ((Path.userHome / ".sbt" / "odo.credentials") :: Nil)
    .filter(_.exists())
    .map(Credentials.apply),
  pgpSecretRing := Path.userHome / ".gnupg" / "secring.gpg",
  organization := "org.manatki",
  version := {
    val branch = git.gitCurrentBranch.value
    if (branch == "master") publishVersion
    else s"$publishVersion-$branch-SNAPSHOT"
  },
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)
