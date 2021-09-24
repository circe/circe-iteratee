ThisBuild / organization := "io.circe"
ThisBuild / crossScalaVersions := Seq("2.12.14", "2.13.6", "3.0.2")
ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    List("clean", "coverage", "test", "coverageReport", "scalafmtCheckAll"),
    id = None,
    name = Some("Test")
  ),
  WorkflowStep.Use(
    UseRef.Public(
      "codecov",
      "codecov-action",
      "v1"
    )
  )
)

val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen"
)

val circeVersion = "0.14.1"
val iterateeVersion = "0.20.0"
val previousCirceIterateeVersion = "0.12.0"

val scalaTestVersion = "3.2.9"
val scalaTestPlusVersion = "3.2.10.0"

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

val baseSettings = Seq(
  scalacOptions ++= compilerOptions,
  scalacOptions ++= (
    if (priorTo2_13(scalaVersion.value))
      Seq(
        "-Xfuture",
        "-Yno-adapted-args",
        "-Ywarn-unused-import"
      )
    else
      Seq(
        "-Ywarn-unused:imports"
      )
  ),
  Compile / console / scalacOptions ~= {
    _.filterNot(Set("-Ywarn-unused-import"))
  },
  Test / console / scalacOptions ~= {
    _.filterNot(Set("-Ywarn-unused-import"))
  },
  coverageHighlighting := true,
  coverageEnabled := (
    if (!scalaVersion.value.startsWith("2.13")) false else coverageEnabled.value
  ),
  Compile / scalastyleSources ++= (Compile / unmanagedSourceDirectories).value
)

val allSettings = baseSettings ++ publishSettings

val docMappingsApiDir = settingKey[String]("Subdirectory in site target directory for API docs")

val iteratee = project
  .in(file("."))
  .settings(allSettings)
  .settings(
    moduleName := "circe-iteratee",
    mimaPreviousArtifacts := Set("io.circe" %% "circe-iteratee" % previousCirceIterateeVersion),
    libraryDependencies ++= Seq(
      "io.iteratee" %% "iteratee-core" % iterateeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
      "io.circe" %% "circe-testing" % circeVersion % Test,
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % scalaTestPlusVersion % Test
    ),
    ghpagesNoJekyll := true,
    docMappingsApiDir := "api",
    addMappingsToSiteDir(Compile / packageDoc / mappings, docMappingsApiDir)
  )

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releaseVcsSign := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/circe/circe-iteratee")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else
      Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
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
