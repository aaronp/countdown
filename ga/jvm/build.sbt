releasePublishArtifactsAction := PgpKeys.publishSigned.value
publishConfiguration := publishConfiguration.value.withOverwrite(
  true)
publishLocalConfiguration := publishLocalConfiguration.value
  .withOverwrite(true)
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(sbt.Path.userHome / ".sbt" / ".credentials")

pomExtra := {
  <url>https://github.com/aaronp/gaProject</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>aaronp</id>
        <name>aaronp</name>
        <url>http://github.com/aaronp</url>
      </developer>
    </developers>
}
