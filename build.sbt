name := "Manscala"

version := "1.0"

scalaVersion := "2.11.8"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test"


mainClass in (Compile, run) := Some("me.brandonlmorris.manscala.Manscala")
