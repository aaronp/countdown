import java.nio.file.Path

import org.scoverage.coveralls.Imports.CoverallsKeys._
import sbt.KeyRanks
import sbt.Keys.{artifact, libraryDependencies, mainClass, publishMavenStyle}
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtrelease.ReleasePlugin.autoImport.releaseCrossBuild

ThisBuild / organization := "com.github.aaronp"
ThisBuild / scalaVersion := "2.13.3"

val projectName = "ga"
val username = "aaronp"
val scalaThirteen = "2.13.3"
val defaultScalaVersion = scalaThirteen

name := projectName

organization := s"com.github.$username"

enablePlugins(GhpagesPlugin)
enablePlugins(ParadoxPlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(ParadoxMaterialThemePlugin) // see https://jonas.github.io/paradox-material-theme/getting-started.html

scalaVersion := defaultScalaVersion
val scalaVersions = Seq(scalaThirteen)
crossScalaVersions := scalaVersions //, scalaThirteen)

paradoxProperties += ("project.url" -> s"https://$username.github.io/$projectName/docs/current/")

val testDependencies = List(
  "junit" % "junit" % "4.12" % "test",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0" % "test",
  "org.pegdown" % "pegdown" % "1.6.0" % "test"
)

val scalacSettings = List(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:reflectiveCalls", // Allow reflective calls
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",
  "-language:reflectiveCalls", // Allow reflective calls
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  //"-Xlog-implicits",
  "-Xfuture" // Turn on future language features.
)

val commonSettings: Seq[Def.Setting[_]] = Seq(
  //version := parentProject.settings.ver.value,
  organization := s"com.github.${username}",
  scalaVersion := defaultScalaVersion,
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  autoAPIMappings := true,
  exportJars := false,
  crossScalaVersions := scalaVersions,
  libraryDependencies ++= testDependencies,
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= scalacSettings,
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := s"${projectName}.build",
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case str if str.contains("application.conf") => MergeStrategy.discard
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
  // see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
  //(testOptions in Test) += (Tests.Argument(TestFrameworks.ScalaTest, "-h", s"target/scalatest-reports-${name.value}", "-oN"))
)
coverallsTokenFile := Option(
  (sbt.io.Path.userHome / ".sbt" / ".coveralls.countdown").asPath.toString)

val gaProject = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(JVMPlatform)
  .in(file("ga"))
  .jvmSettings(commonSettings: _*)
  .jvmSettings(
    publishMavenStyle := true,
    releaseCrossBuild := true,
    coverageMinimum := 90,
    coverageFailOnMinimum := true,
    addArtifact(Artifact(projectName, "assembly"),
                sbtassembly.AssemblyKeys.assembly),
    artifact in (Compile, assembly) ~= { art =>
      art.withClassifier(Some("assembly"))
    },
    mainClass in (Compile, run) := Some("countdown.rest.Main"),
    mainClass in (assembly) := Some("countdown.rest.Main")
  )
  .jvmSettings(libraryDependencies ++= List(
    "com.typesafe" % "config" % "1.3.4",
    "com.github.aaronp" %% "args4c" % "0.7.0",
    "com.github.aaronp" %% "eie" % "1.0.0",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "io.circe" %% "circe-generic" % "0.13.0",
    "org.http4s" %% "http4s-circe" % "0.21.7",
    "org.http4s" %% "http4s-core" % "0.21.7",
    "org.http4s" %% "http4s-dsl" % "0.21.7",
    "org.http4s" %% "http4s-blaze-server" % "0.21.7"
  ))
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %%% "cats-core" % "2.2.0"
    )
  )
  .jsSettings(
    libraryDependencies ++= List(
      "com.lihaoyi" %%% "scalatags" % "0.9.2",
      "com.lihaoyi" %%% "scalarx" % "0.4.3",
      "org.scalatest" %%% "scalatest" % "3.2.2" % "test"
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

siteSourceDirectory := target.value / "paradox" / "site" / "main"

siteSubdirName in SiteScaladoc := "api/latest"

git.remoteRepo := s"git@github.com:$username/countdown.git"
ghpagesNoJekyll := true

test in assembly := {}

lazy val makePage =
  taskKey[Unit]("Puts the javascript and html resources together")
    .withRank(KeyRanks.APlusTask)

makePage := {
  import eie.io._
  val jsArtifacts = {
    val path: Path = (fullOptJS in (gaProjectJS, Compile)).value.data.asPath
    val dependencyFiles =
      path.getParent.find(_.fileName.endsWith("-jsdeps.js")).toList
    path :: dependencyFiles
  }
  val jsResourceDir = (resourceDirectory in (gaProjectJS, Compile)).value.toPath

  val targetDir = (baseDirectory.value / "target" / "page").toPath.mkDirs()

  val sharedResourceDir =
    (baseDirectory.value / "ga" / "shared" / "src" / "main" / "resources").toPath

  val sharedJsLibs =
    List("springy.js", "springyui.js", "jquery.min.js", "springy.LICENSE")
      .map(f => sharedResourceDir.resolve(f))

  // always copy/clobber these
  val clobberFiles = (jsResourceDir.children ++ jsArtifacts).map { file =>
    val to = targetDir.resolve(file.fileName)
    sLog.value.info(s"""cp ${file.fileName} to $to""".stripMargin)
    (file.toFile, to.toFile)
  }
  IO.copy(clobberFiles, CopyOptions().withOverwrite(true))

  // copy these on demand
  val onDemand = (sharedJsLibs).map { file =>
    val tgt = targetDir.resolve(file.fileName)
    (file.toFile, tgt.toFile)
  }
  IO.copy(onDemand, CopyOptions().withOverwrite(false))
}

ThisBuild / buildInfoKeys := Seq[BuildInfoKey](name,
                                               version,
                                               scalaVersion,
                                               sbtVersion)
ThisBuild / buildInfoPackage := "countdown.build"

// see http://scalameta.org/scalafmt/
scalafmtOnCompile in ThisBuild := true
scalafmtVersion in ThisBuild := "1.4.0"

// see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
testOptions in Test += (Tests
  .Argument(TestFrameworks.ScalaTest, "-h", s"target/scalatest-reports", "-oN"))
