name := "csa"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile, run) := Some("cli.Entry")

scalacOptions += "-target:jvm-1.8"

libraryDependencies += "com.nrinaudo" %% "kantan.csv" % "0.1.13"