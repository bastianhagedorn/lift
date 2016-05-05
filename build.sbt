name := "Apart"

version := "1.0"

scalaVersion := "2.11.8"

compile <<= (compile in Compile) dependsOn compileSkelcl

lazy val compileSkelcl = taskKey[Unit]("Updates and builds the SkelCL submodule.")

compileSkelcl := {
  import language.postfixOps
  import scala.sys.process._
  "echo y" #| "./skelcl.sh" !
}

scalacOptions ++= Seq("-Xmax-classfile-name", "100")

// Executor path
javaOptions += "-Djava.library.path=" + baseDirectory(_ / "lib/SkelCL/build/executor").value

// Main sources
scalaSource in Compile <<= baseDirectory(_ / "src/main")
javaSource in Compile <<= baseDirectory(_ / "src/main")

// Test sources
scalaSource in Test <<= baseDirectory(_ / "src/test")
javaSource in Test <<= baseDirectory(_ / "src/test")

// Scala libraries
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.8"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4"

libraryDependencies += "jline" % "jline" % "2.12.1"

// JUnit
libraryDependencies += "junit" % "junit" % "4.11"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

// TODO: Pick one for argument parsing
libraryDependencies += "commons-cli" % "commons-cli" % "1.3.1"
libraryDependencies += "org.clapper" %% "argot" % "1.0.3"

// Logging
libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"

scalacOptions in (Compile,doc) := Seq("-implicits", "-diagrams")

// Build ArithExpr
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/ArithExpr/src/main/"
unmanagedSourceDirectories in Test += baseDirectory.value / "lib/ArithExpr/src/main/"

ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "<empty>;benchmarks.*;.*Test.*;junit.*;.*interop.*;.*arithmetic.*;.*testing.*"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-a")

fork := true
