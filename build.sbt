val Http4sVersion = "0.18.0"
val Http4sRhoVersion = "0.18.0-M2"
val Specs2Version = "4.0.2"
val LogbackVersion = "1.2.3"
val MonixVersion = "3.0.0-M3"

lazy val root = (project in file("."))
  .settings(
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
    organization := "com.almostfunctional",
    name := "htplay",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.6",
    scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions", "-Ypartial-unification"),
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "org.http4s"      %% "rho-core"          % Http4sRhoVersion,

      "io.monix" %% "monix-eval" % MonixVersion % Test,
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,

      "org.scalatest" %% "scalatest" % "3.0.3" % Test,
    )
  )

