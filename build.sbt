name := "scala-stuff"

organization := "nl.rintcius"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M1" % "test"

libraryDependencies += "org.specs2" %% "specs2" % "1.13" % "test"

credentials += Credentials(Path.userHome / ".m2" / ".credentials")

publishTo <<= version {
      v: String =>
        val nexus = "http://localhost:8081/"
        if (v.trim.endsWith("SNAPSHOT")) {
          Some("snapshots" at nexus + "nexus/content/repositories/snapshots")
        }
        else {
          Some("releases" at nexus + "nexus/content/repositories/releases")
        }
    }

publishMavenStyle := true

pomIncludeRepository := {
  x => false
}

seq(aetherPublishSettings: _*)

testOptions in Test += Tests.Argument("junitxml")

scalacOptions += "-Xcheck-null"