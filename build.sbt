name := "runDMV"

version := "0.2-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "Local Ivy repository" at "/home/jpate/.ivy2/local/",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "net.sf.jopt-simple" % "jopt-simple" % "4.0-beta1",
  "com.typesafe.akka" % "akka-actor" % "2.0.3",
  "org.scalala" % "scalala_2.9.1" % "1.0.0.RC2",
  "predictabilityparsing" %% "predictabilityparsing" % "0.02-SNAPSHOT"
)


