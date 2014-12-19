import sbt._
import Keys._

object WispBuild extends Build {

  def sharedSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.10.4", "2.11.4"),
    organization := "com.quantifind",
    scalacOptions := Seq("-deprecation", "-unchecked", "-optimize"),
  	retrieveManaged := true,
  	transitiveClassifiers in Scope.GlobalScope := Seq("sources"),
  	resolvers ++= Seq(
  		"sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  		"sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases"
    ),

  	libraryDependencies ++= Seq(
  		"org.scalatest" %% "scalatest" % "2.2.1" % "test"
    )
  )

	lazy val wisp = Project("wisp", file("core"), settings = wispSettings)

	def wispSettings = sharedSettings ++ Seq(
		name := "Wisp",
		libraryDependencies ++= Seq(
		  // Do we need all these jackson imports?
			"com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.4.0",
			"com.fasterxml.jackson.core" % "jackson-annotations" % "2.4.0",
			"com.fasterxml.jackson.module" % "jackson-module-jsonSchema" % "2.4.0",
			"com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.4.0",
			"net.databinder" %% "unfiltered-filter" % "0.8.3",
			"net.databinder" %% "unfiltered-jetty" % "0.8.3",
			"com.quantifind" %% "sumac" % "0.3.0",
                        "org.apache.commons" % "commons-math3" % "3.1"
		)
	)
}
