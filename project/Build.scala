import sbt._
import Keys._

object PoppyBuild extends Build {

  def sharedSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.3",
    organization := "com.quantifind",
    scalacOptions := Seq("-deprecation", "-unchecked", "-optimize"),
  	retrieveManaged := true,
  	transitiveClassifiers in Scope.GlobalScope := Seq("sources"),
  	resolvers ++= Seq(
  		"sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  		"sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases"
    ),

  	libraryDependencies ++= Seq(
  		"org.scalatest" %% "scalatest" % "1.9.1" % "test"
    )
  )

	lazy val poppy = Project("poppy", file("core"), settings = poppySettings)

	def poppySettings = sharedSettings ++ Seq(
		name := "Poppy",
		libraryDependencies ++= Seq(
		  // Do we need all these jackson imports?
			"com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.2.2",
			"com.fasterxml.jackson.core" % "jackson-annotations" % "2.2.2",
			"com.fasterxml.jackson.module" % "jackson-module-jsonSchema" % "2.2.2",
			"com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.2.2",
			// Do we neet all these unfiltered imports?
			"net.databinder" %% "unfiltered-filter" % "0.6.7",
			"net.databinder" %% "unfiltered-jetty" % "0.6.7",
			"net.databinder" %% "unfiltered-json" % "0.6.7",
			"com.quantifind" %% "sumac" % "0.3.0",
                        "org.apache.commons" % "commons-math3" % "3.1"
		)
	)
}
