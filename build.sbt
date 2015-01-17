lazy val root: Project = (project in file(".")).
  settings(
    name := "fpinscala",
    scalaVersion := "2.11.4",
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test"
  ).
  settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.1.6" % "test"
  ))
