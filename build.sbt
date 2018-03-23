import com.typesafe.sbt.SbtGit.git
import jdk.nashorn.internal.objects.Global
import org.typelevel.Dependencies._


addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

val apache2 = "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")
val gh = GitHubSettings(org = "lukajcb", proj = "cats-uio", publishOrg = "org.typelevel", license = apache2)
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

lazy val commonSettings = sharedCommonSettings ++ Seq(
  parallelExecution in Test := false,
  crossScalaVersions := Seq(vAll.vers("scalac_2.11"), scalaVersion.value)
) ++ scalacAllSettings ++ unidocCommonSettings ++
  addCompilerPlugins(vAll, "kind-projector")

lazy val commonJsSettings = Seq()

lazy val commonJvmSettings = Seq()

lazy val publishSettings = sharedPublishSettings(gh, devs) ++ credentialSettings ++ sharedReleaseProcess

lazy val scoverageSettings = sharedScoverageSettings(60)

