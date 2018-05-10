name := "rexplector"

version := "0.1"

scalaVersion := "2.12.6"

val circeVersion = "0.9.3"
libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
).map(_ % circeVersion)