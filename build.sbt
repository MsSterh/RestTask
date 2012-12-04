name := "RestTask application"

version := "0.0.1"

organization := "com.anahoret"

scalaVersion := "2.9.2"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"      at "http://oss.sonatype.org/content/repositories/releases"
                )

seq(com.github.siasia.WebPlugin.webSettings :_*)

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.5-M3"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mongodb-record"% liftVersion,
    "net.databinder.dispatch" %% "dispatch-core" % "0.9.4",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.6"
  )
}

port in container.Configuration := 8080
