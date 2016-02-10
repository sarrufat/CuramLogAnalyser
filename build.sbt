


scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"


resolvers += "Scaladin Snapshots" at "http://henrikerola.github.io/repository/snapshots/"

libraryDependencies ++= Seq(
  "org.vaadin.addons" %% "scaladin" % "3.2-SNAPSHOT",
  "com.vaadin" % "vaadin-server" % "7.5.10",
  "com.vaadin" % "vaadin-client-compiled" % "7.5.10",
  "com.vaadin" % "vaadin-themes" % "7.5.10"
)

enablePlugins(TomcatPlugin)

containerPort := 9090


vaadinWebSettings

vaadinThemes  := Seq("tests-valo-facebook")