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
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
  )
