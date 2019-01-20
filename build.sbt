lazy val root = (project in file("."))
  .settings(
    name := "RedactExample",
    scalaVersion := "2.12.7",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3"
    )
  )
