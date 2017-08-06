import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
    libraryDependencies += "org.apache.commons" % "commons-email" % "1.4",
    libraryDependencies += "joda-time" % "joda-time" % "2.9.4",
    libraryDependencies += "org.joda" % "joda-convert" % "1.8.1",
    libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.2",
    unmanagedJars in Compile += file("lib/scalar-model_2.12-5.5.0.11.jar"),
    unmanagedJars in Compile += file("lib/scalar-utils_2.12-5.5.0.11.jar")
  )
