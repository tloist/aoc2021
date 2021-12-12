import Dependencies._
val scala3Version = "3.1.0"
scalacOptions ++= (
  Seq(
    "-encoding", "UTF-8",
    "-source", "future"
  )
)

name := "Advent of Code 2021"

lazy val day01 = dayProject(1, "Sonar Sweep")
lazy val day02 = dayProject(2, "Dive", Seq(catsParse))
lazy val day03 = dayProject(3, "Binary Diagnostic", Seq(catsParse))
lazy val day04 = dayProject(4, "Giant Squid", Seq(catsParse))
lazy val day05 = dayProject(5, "Hydrothermal Venture", Seq(catsParse))
lazy val day06 = dayProject(6, "Lanternfish", Seq(catsParse))
lazy val day07 = dayProject(7, "The Treachery of Whales", Seq(catsParse))
lazy val day08 = dayProject(8, "Seven Segment Search", Seq(catsParse))
lazy val day09 = dayProject(9, "Smoke Basin", Seq(catsParse))
lazy val day10 = dayProject(10, "Syntax Scoring", Seq(catsParse))
lazy val day11 = dayProject(11, "Dumbo Octopus")
lazy val day12 = dayProject(12, "Passage Pathing", Seq(catsParse))

lazy val common = project
  .in(file("days/common"))
  .settings(
    name := f"Advent-of-Code 2021: Commons",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      betterFiles,
      catsParse,
      mUnit % "test"
    )
  )

def dayProject(day: Int, title: String = ""): Project = Project.apply(f"day_$day%02d", file(f"days/$day%02d"))
  .settings(
    name := f"AoC Day $day%2d" + (if (title.nonEmpty) s" - $title" else ""),
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      cats,
      mUnit % "test"
    )
  )
  .dependsOn(common % "compile->compile;test->test")

def dayProject(day: Int, title: String, additionalDependencies: Seq[ModuleID]): Project  =
  dayProject(day, title).settings(
    libraryDependencies ++= additionalDependencies
  )
