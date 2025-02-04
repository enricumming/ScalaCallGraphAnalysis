name := """CallGraphProject"""

version := "1.0"

//lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.10"

libraryDependencies += "org.scalameta" %% "scalameta" % "4.9.7"

addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.5.13" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
