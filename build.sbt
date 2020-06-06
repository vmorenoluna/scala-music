name := "scala-music"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.17.0-M1",
  "org.scalatest" %% "scalatest" % "3.1.0-SNAP13" % "test",
  "org.typelevel" %% "cats-core" % "2.0.0"
)
