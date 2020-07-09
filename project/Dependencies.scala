import sbt._

object Version {
  val json4sVersion = "3.6.6"
}

object Artifacts {
  import Version._

  val json4sNative  = "org.json4s" %% "json4s-native"  % json4sVersion
  val json4sJackson = "org.json4s" %% "json4s-jackson" % json4sVersion
  // HTTP client
  val requests = "com.lihaoyi" %% "requests" % "0.5.1"

  val scalaTest = "org.scalatest"                %% "scalatest" % "3.2.0" % "test"
  val sttp      = "com.softwaremill.sttp.client" %% "core"      % "2.2.1"

  val akkaHttp   = "com.typesafe.akka" %% "akka-http"   % "10.1.12"
  val akkaStream = "com.typesafe.akka" %% "akka-stream" % "2.5.26"
}

object Dependencies {
  import Artifacts._

  val depends = Seq(
    json4sNative,
    json4sJackson,
    scalaTest,
    requests,
    sttp,
    akkaHttp,
    akkaStream
  )

}
