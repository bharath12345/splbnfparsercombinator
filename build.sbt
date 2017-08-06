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
    //libraryDependencies += "com.glassbeam" %% "scalar-model" % "5.5.0.11" % "provided"
    unmanagedJars in Compile += file("lib/scalar-model_2.12-5.5.0.11.jar"),
    unmanagedJars in Compile += file("lib/scalar-utils_2.12-5.5.0.11.jar")
  )
