import Dependencies._

val akkaVersion = "2.4.17"

val libs = List(
  scalaTest % Test,
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.apache.commons" % "commons-email" % "1.4",
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8.1",
  "org.apache.httpcomponents" % "httpclient" % "4.5.2",
  "io.dropwizard.metrics" % "metrics-core" % "3.1.2",
  "nl.grons" %% "metrics-scala" % "3.5.5",
  "com.typesafe.akka" %% "akka-actor" % akkaVersion
)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies ++= libs,
    unmanagedJars in Compile += file("lib/scalar-model_2.12-5.5.0.11.jar"),
    unmanagedJars in Compile += file("lib/scalar-utils_2.12-5.5.0.11.jar")
  )
