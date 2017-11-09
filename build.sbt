organization in ThisBuild := "io.circe"

val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Xfuture"
)

val circeVersion = "0.9.0-M1"
val iterateeVersion = "0.13.0"
val previousCirceIterateeVersion = "0.8.0"

val baseSettings = Seq(
  scalacOptions ++= compilerOptions,
  scalacOptions in (Compile, console) ~= {
    _.filterNot(Set("-Ywarn-unused-import"))
  },
  scalacOptions in (Test, console) ~= {
    _.filterNot(Set("-Ywarn-unused-import"))
  },
  coverageHighlighting := true,
  coverageScalacPluginVersion := "1.3.0",
  (scalastyleSources in Compile) ++= (unmanagedSourceDirectories in Compile).value
)

val allSettings = baseSettings ++ publishSettings

val docMappingsApiDir = settingKey[String]("Subdirectory in site target directory for API docs")

val iteratee = project.in(file("."))
.settings(allSettings)
.settings(
  moduleName := "circe-iteratee",
  mimaPreviousArtifacts := Set("io.circe" %% "circe-iteratee" % previousCirceIterateeVersion),
  libraryDependencies ++= Seq(
    "io.iteratee" %% "iteratee-core" % iterateeVersion,
    "io.circe" %% "circe-jawn" % circeVersion,
    "io.circe" %% "circe-testing" % circeVersion % "test"
  ),
  ghpagesNoJekyll := true,
  docMappingsApiDir := "api",
  addMappingsToSiteDir(mappings in (Compile, packageDoc), docMappingsApiDir)
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/circe/circe-iteratee")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  /* Someday maybe Scaladoc will actually work on package object-only projects.
  autoAPIMappings := true,
  apiURL := Some(url("https://circe.github.io/circe-iteratee/api/")),
  */
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/circe/circe-iteratee"),
      "scm:git:git@github.com:circe/circe-iteratee.git"
    )
  ),
  developers := List(
    Developer(
      "travisbrown",
      "Travis Brown",
      "travisrobertbrown@gmail.com",
      url("https://twitter.com/travisbrown")
    )
  )
)

credentials ++= (
  for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    username,
    password
  )
).toSeq
