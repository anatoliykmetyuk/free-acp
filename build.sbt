val ScalaVer = "2.12.1"

val Cats          = "0.8.1"
val Shapeless     = "2.3.2"
val ScalaCheck    = "1.13.4"
val KindProjector = "0.9.3"

lazy val commonSettings = Seq(
  name    := "free-acp"
, version := "0.1.0"
, scalaVersion := ScalaVer
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

, testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", "10000", "-workers", "10", "-verbosity", "1")
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    initialCommands := "import freeacp._; import Main._"
  )
