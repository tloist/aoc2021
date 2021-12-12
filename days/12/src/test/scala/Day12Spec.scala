import munit.FunSuite
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

extension (caves: Set[(String, String)])
  def toCaveSystem: CaveSystem = CaveSystem(caves.map(t => (Cave(t._1), Cave(t._2))).toSet)

class Day12Spec extends FunSuite {

  val smallExample: String =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end
      |""".stripMargin

  val smallCaveSystem: CaveSystem = Set(
    "start" -> "A",
    "start" -> "b",
    "A" -> "c",
    "A" -> "b",
    "b" -> "d",
    "A" -> "end",
    "b" -> "end"
  ).toCaveSystem

  test("Parser for small example") {
    checkParseFunc(Parser.caveSystem, smallExample) { caveSystem =>
      assertEquals(caveSystem.size, 7)
    }
  }

  test("Small cave system: Start is connected to A and b") {
    assertEquals(smallCaveSystem.allConnectionsFor(Cave.start), Set(Cave("A"), Cave("b")))
  }

  test("Small cave system: A is connected to start, c, b and end") {
    assertEquals(smallCaveSystem.allConnectionsFor(Cave("A")), Set(Cave.start, Cave("c"), Cave("b"), Cave.end))
  }

  test("Small cave system: Check all paths from start to end") {
    assertEquals(smallCaveSystem.allTripsPart1From(Cave.start), List(
      List("start","A","b","A","c","A","end"),
      List("start","A","b","A","end"),
      List("start","A","b","end"),
      List("start","A","c","A","b","A","end"),
      List("start","A","c","A","b","end"),
      List("start","A","c","A","end"),
      List("start","A","end"),
      List("start","b","A","c","A","end"),
      List("start","b","A","end"),
      List("start","b","end"),
    ).map(_.map(Cave.apply)).map(CaveTrip.apply).toSet)
  }

  val slightlyLargerCaveSystem = Set(
    "dc" -> "end",
    "HN" -> "start",
    "start" -> "kj",
    "dc" -> "start",
    "dc" -> "HN",
    "LN" -> "dc",
    "HN" -> "end",
    "kj" -> "sa",
    "kj" -> "HN",
    "kj" -> "dc",
  ).toCaveSystem

  test("Slightly larger cave system: Check all paths from start to end") {
    assertEquals(slightlyLargerCaveSystem.allTripsPart1From(Cave.start), List(
      List("start","HN","dc","HN","end"),
      List("start","HN","dc","HN","kj","HN","end"),
      List("start","HN","dc","end"),
      List("start","HN","dc","kj","HN","end"),
      List("start","HN","end"),
      List("start","HN","kj","HN","dc","HN","end"),
      List("start","HN","kj","HN","dc","end"),
      List("start","HN","kj","HN","end"),
      List("start","HN","kj","dc","HN","end"),
      List("start","HN","kj","dc","end"),
      List("start","dc","HN","end"),
      List("start","dc","HN","kj","HN","end"),
      List("start","dc","end"),
      List("start","dc","kj","HN","end"),
      List("start","kj","HN","dc","HN","end"),
      List("start","kj","HN","dc","end"),
      List("start","kj","HN","end"),
      List("start","kj","dc","HN","end"),
      List("start","kj","dc","end"),
    ).map(_.map(Cave.apply)).map(CaveTrip.apply).toSet)
  }

  val evenLargerCaveSystem = Set(
    "fs" -> "end",
    "he" -> "DX",
    "fs" -> "he",
    "start" -> "DX",
    "pj" -> "DX",
    "end" -> "zg",
    "zg" -> "sl",
    "zg" -> "pj",
    "pj" -> "he",
    "RW" -> "he",
    "fs" -> "DX",
    "pj" -> "RW",
    "zg" -> "RW",
    "start" -> "pj",
    "he" -> "WI",
    "zg" -> "he",
    "pj" -> "fs",
    "start" -> "RW",
  ).toCaveSystem

  test("Even larger cave system: Check all paths from start to end") {
    assertEquals(evenLargerCaveSystem.allTripsPart1From(Cave.start).size, 226)
  }

  test("Part2: Small Cave -- Check all paths from start to end") {
    assertEquals(smallCaveSystem.allTripsPart2From(Cave.start).size, 36)
  }

  test("Part2: Slightly Larger Cave -- Check all paths from start to end") {
    assertEquals(slightlyLargerCaveSystem.allTripsPart2From(Cave.start).size, 103)
  }

  test("Part2: Even Larger Cave -- Check all paths from start to end") {
    assertEquals(evenLargerCaveSystem.allTripsPart2From(Cave.start).size, 3509)
  }

}
