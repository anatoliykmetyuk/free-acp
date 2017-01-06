val ScalaVer    = "2.12.1"
val ThisVersion = "0.1.0"

val Cats          = "0.8.1"
val Shapeless     = "2.3.2"
val ScalaCheck    = "1.13.4"
val KindProjector = "0.9.3"

val ScalaSwing = "2.0.0-M2"

val ScalacheckMinTests = "100000"

lazy val commonSettings = Seq(
  scalaVersion := ScalaVer
, libraryDependencies ++= Seq(
    "org.typelevel"  %% "cats"       % Cats
  , "com.chuusai"    %% "shapeless"  % Shapeless
  , "org.scalacheck" %% "scalacheck" % ScalaCheck % "test"
  )
, addCompilerPlugin("org.spire-math" %% "kind-projector" % KindProjector)
, scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-unchecked",
      "-Xlint",
      "-Ywarn-dead-code",
      "-Xfuture")

, testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", ScalacheckMinTests, "-workers", "10", "-verbosity", "1")
)

lazy val root = (project in file("."))
  .aggregate(core, lookupframe)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name    := "free-acp"
  , version := "0.1.0"
  , initialCommands := "import freeacp._; import Main._"
  )

lazy val lookupframe = (project in file("lookupframe"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % ScalaSwing
  )