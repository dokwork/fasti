lazy val `fasti` = (project in file("."))
  .settings(
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
      "com.chuusai"   %% "shapeless" % "2.3.3",
      // tests:
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    )
  )
  .settings(
    coverageMinimum := 90,
    coverageFailOnMinimum := false
  )
