name := "neuralnet"

organization := "com.devdiscoveries"

version := "0.1"

scalaVersion := "2.11.5"

scalacOptions += "-target:jvm-1.7"

scalacOptions += "-feature"
 
scalacOptions += "-deprecation"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.10"

libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.10"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

EclipseKeys.withSource := true

autoAPIMappings := true