name := "runge-kutta-method"

version := "0.1"

scalaVersion := "2.12.7"


libraryDependencies += "org.yaml" % "snakeyaml" % "1.8"

libraryDependencies ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "1.0",

  // Native libraries are not included by default. add this if you want them
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "1.0",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "1.0"
)

libraryDependencies += "com.jakewharton.fliptables" % "fliptables" % "1.1.0"