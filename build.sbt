ThisBuild / intellijPluginName := "intellij-plugin-demo"
ThisBuild / intellijPlatform := IntelliJPlatform.IdeaCommunity
ThisBuild / intellijBuild := "2020.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    intellijPlugins += "org.intellij.scala".toPlugin
  )

lazy val runner = createRunnerProject(root)