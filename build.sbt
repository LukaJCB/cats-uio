import com.typesafe.sbt.SbtGit.git
import jdk.nashorn.internal.objects.Global
import org.typelevel.Dependencies._


addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

val apache2 = "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")
val gh = GitHubSettings(org = "lukajcb", proj = "cats-uio", publishOrg = "com.github.lukajcb", license = apache2)
val devs = Seq(Dev("Luka Jacobowitz", "@lukajcb"))

val vAll = Versions(versions, libraries, scalacPlugins)

lazy val rootSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings
lazy val module = mkModuleFactory(gh.proj, mkConfig(rootSettings, commonJvmSettings, commonJsSettings))
lazy val prj = mkPrjFactory(rootSettings)

lazy val rootPrj = project
  .configure(mkRootConfig(rootSettings,rootJVM))
  .aggregate(rootJVM, rootJS )
  .dependsOn(rootJVM, rootJS)
  .settings(noPublishSettings)


lazy val rootJVM = project
  .configure(mkRootJvmConfig(gh.proj, rootSettings, commonJvmSettings))
  .aggregate(coreJVM, testsJVM, docs)
  .dependsOn(coreJVM, testsJVM)
  .settings(noPublishSettings)


lazy val rootJS = project
  .configure(mkRootJsConfig(gh.proj, rootSettings, commonJsSettings))
  .aggregate(coreJS, testsJS)
  .dependsOn(coreJS, testsJS)
  .settings(noPublishSettings)


lazy val catsEffectVersion = "0.10"
lazy val catsEffectDependency = Seq(
  libraryDependencies += "org.typelevel" %%% "cats-effect" % catsEffectVersion)

lazy val catsEffectLawsDependency = Seq(
  libraryDependencies += "org.typelevel" %%% "cats-effect-laws" % catsEffectVersion)

lazy val catsVersion = "1.1.0"
lazy val catsTestKitDependency = Seq(
  libraryDependencies += "org.typelevel" %%% "cats-testkit" % catsVersion,
  libraryDependencies += "org.typelevel" %%% "cats-laws" % catsVersion)



lazy val core    = prj(coreM)
lazy val coreJVM = coreM.jvm
lazy val coreJS  = coreM.js
lazy val coreM   = module("core", CrossType.Pure)
  .settings(addLibs(vAll, "cats-core"))
  .settings(simulacrumSettings(vAll))
  .settings(catsEffectDependency)


lazy val tests    = prj(testsM)
lazy val testsJVM = testsM.jvm
lazy val testsJS  = testsM.js
lazy val testsM   = module("tests", CrossType.Pure)
  .dependsOn(coreM)
  .settings(noPublishSettings)
  .settings(addTestLibs(vAll, "scalatest"))
  .settings(catsTestKitDependency)
  .settings(catsEffectLawsDependency)


/** Docs - Generates and publishes the scaladoc API documents and the project web site using sbt-microsite.*/
lazy val docs = project.configure(mkDocConfig(gh, rootSettings, commonJvmSettings,
  coreJVM))

lazy val buildSettings = sharedBuildSettings(gh, vAll)

useGpg := false
usePgpKeyHex("7872706ADA343DF6")
pgpPublicRing := baseDirectory.value / "project" / ".gnupg" / "pubring.gpg"
pgpSecretRing := baseDirectory.value / "project" / ".gnupg" / "secring.gpg"
pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)

sonatypeProfileName := organization.value

credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  sys.env.getOrElse("SONATYPE_USER", ""),
  sys.env.getOrElse("SONATYPE_PASS", "")
)

isSnapshot := version.value endsWith "SNAPSHOT"

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

licenses := Seq(apache2)
homepage := Some(url("https://github.com/LukaJCB/cats-uio"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/LukaJCB/cats-uio"),
    "scm:git@github.com:LukaJCB/cats-uio.git"
  ))

developers := List(
  Developer(
    id="LukaJCB",
    name="Luka Jacobowitz",
    email="noreply@jacobowitz.org",
    url=url("https://lukajcb.github.io")
  ))

enablePlugins(GitVersioning)

/* The BaseVersion setting represents the in-development (upcoming) version,
 * as an alternative to SNAPSHOTS.
 */
git.baseVersion := "3.0.0"

val ReleaseTag = """^v([\d\.]+)$""".r
git.gitTagToVersionNumber := {
  case ReleaseTag(v) => Some(v)
  case _ => None
}

git.formattedShaVersion := {
  val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, git.uncommittedSignifier.value)

  git.gitHeadCommit.value map { _.substring(0, 7) } map { sha =>
    git.baseVersion.value + "-" + sha + suffix
  }
}

lazy val commonSettings = sharedCommonSettings ++ Seq(
  parallelExecution in Test := false,
  crossScalaVersions := Seq(vAll.vers("scalac_2.11"), scalaVersion.value)
) ++ scalacAllSettings ++ unidocCommonSettings ++
  addCompilerPlugins(vAll, "kind-projector")

lazy val commonJsSettings = Seq()

lazy val commonJvmSettings = Seq()

lazy val publishSettings = sharedPublishSettings(gh, devs) ++ credentialSettings ++ sharedReleaseProcess

lazy val scoverageSettings = sharedScoverageSettings(60)

