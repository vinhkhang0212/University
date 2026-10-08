ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "MyTDProject"
  )

libraryDependencies += "org.scalafx" % "scalafx_3" % "20.0.0-R31"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.16"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scalafx" % "scalafx-extras_3" % "0.8.0"
libraryDependencies += "org.controlsfx" % "controlsfx" % "11.1.2"
libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_3" % "1.0.4"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.0.0"