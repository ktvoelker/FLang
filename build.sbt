
name := "FLang"

version := "0.0.1"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.4"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation"
)

