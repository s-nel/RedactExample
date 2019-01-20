lazy val root = (project in file("."))
  .settings(
    name := "RedactExample",
    scalaVersion := "2.12.7",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "io.circe" %% "circe-core" % "0.10.0",
      "io.circe" %% "circe-generic" % "0.10.0",
      "io.circe" %% "circe-parser" % "0.10.0"
    )
  )
