import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"

  lazy val monix = "io.monix" %% "monix" % "3.0.0-RC1"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.0.0"
  lazy val catsFree = "org.typelevel" %% "cats-free" % "1.1.0"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "1.1.0"
  lazy val mouse    = "org.typelevel" %% "mouse" % "0.19"
  lazy val fs2 = "co.fs2" %% "fs2-io" % "1.0.0-M5"
  lazy val zio =  "org.scalaz" %% "scalaz-zio" % "0.1.0-dc8b6a3"
  lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.2.20"
  lazy val squants = "org.typelevel"  %% "squants"  % "1.3.0"
  lazy val finchCore = "com.github.finagle" %% "finch-core" % "0.17.0"
  lazy val finchCirce = "com.github.finagle" %% "finch-circe" % "0.17.0"
  lazy val circeGeneric =  "io.circe" %% "circe-generic" % "0.9.0"
  lazy val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.13.0"
  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  lazy val matryoshka = "com.slamdata" %% "matryoshka-core" % "0.18.3"
  lazy val akkaStreams = "com.typesafe.akka" %% "akka-stream" % "2.5.16"
  lazy val akkaHttp = "com.typesafe.akka" %% "akka-http" % "10.1.4"
  lazy val circeAkkaHttp = "de.heikoseeberger" %% "akka-http-circe" % "1.22.0"

}
