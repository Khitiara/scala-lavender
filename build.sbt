name := "scala-lavender"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions ++= Seq("-Ypartial-unification", "-deprecation")

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.2",
  "org.typelevel" %% "cats-core" % "1.4.0",
  "org.typelevel" %% "cats-free" % "1.4.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.beachape" %% "enumeratum" % "1.5.13",
  "com.beachape" %% "enumeratum-cats" % "1.5.14",
  "org.typelevel" %% "cats-effect" % "1.0.0",
  "org.tpolecat" %% "atto-core" % "0.6.4",
  "org.tpolecat" %% "atto-fs2" % "0.6.4",
  "co.fs2" %% "fs2-core" % "1.0.0",
  "co.fs2" %% "fs2-io" % "1.0.0",

  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
