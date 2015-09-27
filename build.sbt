name := "qap"

organization := "me.stojan"

version := "0.0.1-SNAPSHOT"

publishMavenStyle := true

scalaVersion := "2.11.7"

homepage := Some(url("https://github.com/hf/qap"))

licenses := Seq("MIT" -> url("https://github.com/hf/qap/blob/master/LICENSE.txt"))

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"

libraryDependencies += "me.stojan" %% "polynome" % "0.0.1-SNAPSHOT"

libraryDependencies += "me.stojan" %% "reunion" % "0.0.1-SNAPSHOT"
