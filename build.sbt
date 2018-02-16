val Http4sVersion = "0.18.0"
val Specs2Version = "4.0.2"
val LogbackVersion = "1.2.3"
val MonixVersion = "3.0.0-M3"

lazy val root = (project in file("."))
  .settings(
    organization := "com.skedulo",
    name := "htplay",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "org.specs2"     %% "specs2-core"          % Specs2Version % "test",

      "io.monix" %% "monix" % MonixVersion,
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion
    )
  )

