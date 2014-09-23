name := "overpascala"

version := "1.0"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.2.9",
  "org.json4s" %% "json4s-ext" % "3.2.9",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "org.scalaz" %% "scalaz-geo" % "6.0.4",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "io.github.karols" %% "units" % "0.2.0",
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.1",
  "org.scalanlp" %% "breeze" % "0.9"
)
