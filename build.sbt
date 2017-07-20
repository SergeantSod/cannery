name := "cannery"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.18",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)
