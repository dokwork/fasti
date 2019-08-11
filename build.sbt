lazy val `fasti` = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    coverageMinimum := 90,
    coverageFailOnMinimum := true
  )
  .aggregate(`fasti-core`, `fasti-persistence`)

lazy val `fasti-core` = (project in file("core"))
  .settings(commonSettings: _*)

lazy val `fasti-persistence` = (project in file("persistence"))
  .settings(commonSettings: _*)
  .dependsOn(`fasti-core` % "test->test;compile->compile")

lazy val `example` = (project in file("example"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.11.1",
      "io.circe" %% "circe-generic" % "0.11.1",
      "org.typelevel" %% "cats-effect" % "1.3.1"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .dependsOn(`fasti`, `fasti-persistence`)

def commonSettings = Seq(
  organization := "ru.dokwork",
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq(
    "-encoding",
    "utf-8",
    "-target:jvm-1.8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xexperimental",
    "-Xlint",
    "-Ywarn-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-nullary-override",
    "-Ywarn-unused",
    "-Xfatal-warnings",
    "-language:higherKinds"
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0"),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.5.0",
    "com.chuusai" %% "shapeless" % "2.3.3",
    // tests:
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  )
)