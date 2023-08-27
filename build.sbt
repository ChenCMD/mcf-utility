name := "MCF Utility"
ThisBuild / version := "0.0.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

ThisBuild / scalafixDependencies ++= Seq(
  "com.github.liancheng" %% "organize-imports" % "0.5.0"
)
semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ykind-projector:underscores",
  "-no-indent"
)

javaOptions ++= Seq(
  "-Xmx4G",
  "-XX:+UseG1GC"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.4.8",
  "org.typelevel" %% "cats-mtl" % "1.3.0",
  "org.typelevel" %% "mouse" % "1.2.1",
  "com.lihaoyi" %% "upickle" % "3.1.0"
)

assembly / assemblyJarName := "McfUtility.jar"
assembly / mainClass := Some("com.github.chencmd.mcfutility.Main")