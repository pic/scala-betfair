seq(githubPagesMavenPublishSettings: _*)

name := "scala-betfair"

organization := "com.github.oxlade39"

version := "1.0"

crossScalaVersions := Seq("2.9.1", "2.9.2")

// Java then Scala for main sources
compileOrder in Compile := CompileOrder.JavaThenScala

githubPagesCheckoutDir := Path.userHome / "proj" / "oxlade39.github.com" / "_site" / "maven"

publishMavenStyle := true

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.1",
  "org.joda" % "joda-convert" % "1.2",
  "org.joda" % "joda-convert" % "1.2",
  "org.specs2" %% "specs2" % "1.9" % "test",
  "org.mockito" % "mockito-all" % "1.9.0" % "test"
)