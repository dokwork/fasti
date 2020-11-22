import sbtrelease.ReleaseStateTransformations._

lazy val scala212               = "2.12.12"
lazy val scala213               = "2.13.3"
lazy val supportedScalaVersions = List(scala212, scala213)

val catsVersion       = "2.2.0"
val catsEffectVersion = "2.2.0"
val circeVersion      = "0.13.0"
val derevoVersion     = "0.11.5"
val shapelessVersion  = "2.3.3"
val scalatestVersion  = "3.2.1"

lazy val `fasti` = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    releaseReadmeFile := Some(baseDirectory.value / "README.md"),
    coverageMinimum := 90,
    coverageFailOnMinimum := true
  )

lazy val `example` = (project in file("example"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-core"            % circeVersion,
      "org.manatki"   %% "derevo-circe-magnolia" % derevoVersion,
      "org.typelevel" %% "cats-effect"           % catsEffectVersion
    ),
    skip in publish := true
  )
  .dependsOn(`fasti`)

def commonSettings = Seq(
  organization := "ru.dokwork",
  scalaVersion := scala213,
  crossScalaVersions := supportedScalaVersions,
  scalacOptions ++= Seq(
    "-encoding",
    "utf-8",
    "-target:jvm-1.8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Xfatal-warnings",
    "-language:higherKinds",
    "-Ymacro-annotations"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "com.chuusai"   %% "shapeless" % shapelessVersion,
    // tests:
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )
)

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  updateReadme,
  commitReadme,
  tagRelease,
  releaseStepCommandAndRemaining("publish"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)
