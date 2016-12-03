import sbt._

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at
    "https://oss.sonatype.org/content/repositories/releases"
)

scalaVersion := "2.11.8"

val monocleVersion = "1.2.2"

val scalazVersion = "7.2.2"

val matryoshkaVersion = "0.11.1"

lazy val http4sVersion = "0.15.0-SNAPSHOT"

libraryDependencies ++= Seq(
//  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test",
  "org.scalaz"                  %%  "scalaz-core"     % scalazVersion,
//  "com.slamdata"                %%  "matryoshka-core" % matryoshkaVersion,
//"org.typelevel"               %%  "cats"            % "0.7.2",
  "org.scalatest" %% "scalatest" % "3.0.1"
)

// for @Lenses macro support
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

// For kind projector, `Either[String, ?]`
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")


initialCommands in console := "import scalaz._, Scalaz._"


scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-encoding", "utf8",
  "-target:jvm-1.7",
  "-feature",
  "-language:implicitConversions",
  "-language:dynamics",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:_",
  "-Yno-adapted-args",
  "-Xlog-reflective-calls",
  "-Xfuture")
