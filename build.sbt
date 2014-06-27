name := "magisterka"

version := "0.1"

libraryDependencies  ++= Seq(
            //"org.apache.spark" %% "spark-core" % "0.9.0-incubating",
            "org.apache.spark" %% "spark-core" % "1.0.0",
            ////"org.scalanlp" %% "breeze-math" % "0.4-SNAPSHOT",
            //"org.scalanlp" % "breeze_2.10" % "0.5.2",
            ////"org.scalanlp" %% "breeze-learn" % "0.4-SNAPSHOT",
            ////"org.scalanlp" %% "breeze-process" % "0.4-SNAPSHOT",
            ////"org.scalanlp" %% "breeze-viz" % "0.4-SNAPSHOT",
            "org.specs2" %% "specs2" % "1.14" % "test",
            "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
            "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
            "com.chuusai" % "shapeless_2.10.4" % "2.0.0"
)


resolvers ++= Seq(
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Akka Repository" at "http://repo.akka.io/releases/"
)

scalacOptions += "-deprecation"

scalacOptions ++= Seq("-Xmax-classfile-name", "254")

scalaVersion := "2.10.4"

