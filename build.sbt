ThisBuild / scalaVersion := "2.12.7"

val monocleVersion     = "1.2.2"
val scalazVersion      = "7.2.10"
val matryoshkaVersion  = "0.11.1"
lazy val http4sVersion = "0.15.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalaz"    %% "scalaz-core"       % scalazVersion,
  "org.scalaz"    %% "scalaz-concurrent" % scalazVersion,
  "org.typelevel" %% "cats"              % "0.7.2",
  "org.scalatest" %% "scalatest"         % "3.0.1",
  "com.chuusai"   %% "shapeless"         % "2.3.2",
  "com.lihaoyi"   % "ammonite"           % "1.6.3-0-c77705a" % "test" cross CrossVersion.full,
  "org.typelevel" %% "cats"              % "0.9.0"
)

// for @Lenses macro support
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

// For kind projector, `Either[String, ?]`
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

initialCommands in console := "import scalaz._, Scalaz._"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-target:jvm-1.7", "-feature", "-language:implicitConversions", "-language:dynamics",
  "-language:postfixOps", "-language:higherKinds", "-language:_", "-Yno-adapted-args", "-Xlog-reflective-calls", "-Xfuture")
//initialCommands in (Test, console) := """ammonite.Main().run()"""
