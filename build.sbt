import org.scoverage.coveralls.Imports.CoverallsKeys._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import eie.io._

ThisBuild / organization := "countdown"
ThisBuild / scalaVersion := "2.12.10"

val projectName = "countdown"
val username = "aaronp"
val scalaTwelve = "2.12.10"
val scalaThirteen = "2.13.0"
val defaultScalaVersion = scalaTwelve

name := projectName

organization := s"com.github.$username"

enablePlugins(GhpagesPlugin)
enablePlugins(ParadoxPlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(ParadoxMaterialThemePlugin) // see https://jonas.github.io/paradox-material-theme/getting-started.html

scalaVersion := defaultScalaVersion
crossScalaVersions := Seq(scalaTwelve) //, scalaThirteen)

paradoxProperties += ("project.url" -> s"https://$username.github.io/$projectName/docs/current/")

val gaProject = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .jsSettings(
    libraryDependencies ++= List(
      "com.lihaoyi" %%% "scalatags" % "0.7.0",
      "com.lihaoyi" %%% "scalarx" % "0.4.0",
      "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
    ))

lazy val gaProjectJVM = gaProject.jvm
lazy val gaProjectJS = gaProject.js

lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(ParadoxPlugin)
  .aggregate(gaProjectJVM, gaProjectJS)
  .settings(
    publish := {},
    publishLocal := {}
  )

Compile / paradoxMaterialTheme ~= {
  _.withLanguage(java.util.Locale.ENGLISH)
    .withColor("blue", "grey")
    //.withLogoIcon("cloud")
    .withRepository(uri(s"https://github.com/$username/$projectName"))
    .withSocial(uri("https://github.com/$username"))
    .withoutSearch()
}

//scalacOptions += Seq("-encoding", "UTF-8")

siteSourceDirectory := target.value / "paradox" / "site" / "main"

siteSubdirName in SiteScaladoc := "api/latest"

libraryDependencies ++= List(
  "com.typesafe" % "config" % "1.3.4",
  "com.github.aaronp" %% "args4c" % "0.7.0",
  "org.typelevel" %% "cats-core" % "2.0.0"
)

libraryDependencies ++= List(
  "org.scalactic" %% "scalactic" % "3.0.8" % "test",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.pegdown" % "pegdown" % "1.6.0" % "test",
  "junit" % "junit" % "4.12" % "test"
)

publishMavenStyle := true
releaseCrossBuild := true
coverageMinimum := 90
coverageFailOnMinimum := true
git.remoteRepo := s"git@github.com:$username/countdown.git"
ghpagesNoJekyll := true
releasePublishArtifactsAction := PgpKeys.publishSigned.value
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)

test in assembly := {}
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

// https://coveralls.io/github/aaronp/countdown
// https://github.com/scoverage/sbt-coveralls#specifying-your-repo-token
coverallsTokenFile := Option(
  (Path.userHome / ".sbt" / ".coveralls.countdown").asPath.toString)

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "countdown.build"

// see http://scalameta.org/scalafmt/
scalafmtOnCompile in ThisBuild := true
scalafmtVersion in ThisBuild := "1.5.1"

// see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
testOptions in Test += (Tests
  .Argument(TestFrameworks.ScalaTest, "-h", s"target/scalatest-reports", "-oN"))

pomExtra := {
  <url>https://github.com/{username}/{projectName}</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>{username}</id>
        <name>{username}</name>
        <url>http://github.com/{username}</url>
      </developer>
    </developers>
}
