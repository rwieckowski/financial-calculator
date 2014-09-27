name := "financial-calculator"

version := "1.0"

scalaVersion := "2.11.2"

//scalacOptions += "-target:jvm-1.7"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.4",
  "org.scalatest" %% "scalatest" % "2.2.2" % "test"
)

