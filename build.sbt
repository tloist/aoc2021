import Dependencies._
val scala3Version = "3.1.0"

name := "Advent of Code 2021"

lazy val day01 = dayProject(1, "Sonar Sweep")


lazy val common = project
  .in(file("days/common"))
  .settings(
    name := f"Advent-of-Code 2021: Commons",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      betterFiles,
      catsParse
    )
  )

def dayProject(day: Int, title: String = "") = Project.apply(f"day_$day%02d", file(f"days/$day%02d"))
  .settings(
    name := f"AoC Day $day%2d" + (if (title.nonEmpty) s" - $title" else ""),
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      cats,
      scalatest % "test"
    )
  )
  .dependsOn(common % "compile->compile;test->test")