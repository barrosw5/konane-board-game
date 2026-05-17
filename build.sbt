ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "69_MartimBarros129873_PedroCoelho129825_JoaoAlmeida129862"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"

libraryDependencies += "org.openjfx" % "javafx-base" % "21.0.2"
libraryDependencies += "org.openjfx" % "javafx-controls" % "21.0.2"
libraryDependencies += "org.openjfx" % "javafx-fxml" % "21.0.2"
libraryDependencies += "org.openjfx" % "javafx-graphics" % "21.0.2"

Compile / unmanagedResources ++= ((Compile / scalaSource).value ** "*.fxml").get
