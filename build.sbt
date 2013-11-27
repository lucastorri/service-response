name := "nsr"

version := "1.0"

scalaVersion := "2.10.1"

scalacOptions := Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.apache.httpcomponents" % "httpasyncclient" % "4.0"
