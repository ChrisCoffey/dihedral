scalaVersion := "2.11.7"

name := "dihedral"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"