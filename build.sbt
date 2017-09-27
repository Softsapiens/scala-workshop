

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)



lazy val commonSettings = Seq(
  organization := "my.playground",
  version := "0.1.0-SNAPSHOT",
  scalaVersion in ThisBuild := "2.12.3",
  scalacOptions ++= Seq(
      "-language:_",
      "-Ypartial-unification",
      "-Xfatal-warnings"
  )
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "Playground",
    libraryDependencies ++= Seq(
      "com.github.mpilquist" %% "simulacrum"  % "0.10.0",
      "com.chuusai" %% "shapeless"   % "2.3.2",
      "org.typelevel" %% "export-hook" % "1.2.0",
      "org.scalaz" %% "scalaz-core"  % "7.2.15",
      "org.scalaz" %% "scalaz-effect"  % "7.2.15",
      "co.fs2" %% "fs2-core" % "0.9.7",
      "io.monix" %% "monix" % "2.3.0"
    ),
    libraryDependencies ++= Seq("com.github.kenbot" %%  "goggles-dsl"     % "1.0",
                                "com.github.kenbot" %%  "goggles-macros"  % "1.0"),
    scalacOptions += "-Yrangepos" // Enables better error messages
      // use sbt <module_name>/test:console to run an ammonite console
    , libraryDependencies ++= Seq(
        "com.lihaoyi" % "ammonite" % "1.0.1" % "test" cross CrossVersion.patch
          // , "com.lihaoyi" % "ammonite-ops" % "1.0.1" % "test" cross CrossVersion.patch
    )
    , initialCommands in (Test, console) := """ammonite.Main().run()"""
  )
