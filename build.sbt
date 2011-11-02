name := "runDMV"

version := "0.1"

libraryDependencies += "net.sf.jopt-simple" % "jopt-simple" % "4.0-beta1"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "se.scalablesolutions.akka" % "akka-actor" % "1.1.3",
  "se.scalablesolutions.akka" % "akka-typed-actor" % "1.1.3",
  "se.scalablesolutions.akka" % "akka-amqp" % "1.1.3",
  "se.scalablesolutions.akka" % "akka-testkit" % "1.1.3"
)


