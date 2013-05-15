name := "runDMV"

version := "0.21-SNAPSHOT"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "Local Ivy repository" at "file://home/jpate/.ivy2/local/",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "net.sf.jopt-simple" % "jopt-simple" % "4.0-beta1",
  "com.typesafe.akka" % "akka-actor" % "2.0.3",
  "predictabilityparsing" %% "predictabilityparsing" % "0.021-SNAPSHOT"
)

publishArtifact in packageDoc := false

scalacOptions ++= Seq( "-deprecation", "-feature" )

