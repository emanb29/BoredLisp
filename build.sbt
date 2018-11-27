name := "BoredLisp"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions += "-Ypartial-unification"


libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0-RC1"
libraryDependencies += "org.tpolecat" %% "atto-core"    % "0.6.4"
//libraryDependencies += "org.tpolecat" %% "atto-refined" % "0.6.4"