import cats._
import cats.data._
import cats.implicits._
import Cave.*

object Cave {
  opaque type Cave = String

  def apply(name: String): Cave = name

  val start: Cave = "start"
  val end: Cave = "end"

  extension (c: Cave)
    def isSmall: Boolean = c.forall(_.isLower)
    def isBig: Boolean = !c.isSmall
}

case class CaveTrip(visited: List[Cave])

case class CaveSystem(connections: Set[(Cave, Cave)]) {
  require(connections.exists(c => c._1 == start || c._2 == start))
  require(connections.exists(c => c._1 == end || c._2 == end))

  def allConnectionsFor(cave: Cave): Set[Cave] = connections
    .filter(c => c._1 == cave || c._2 == cave)
    .flatMap(c => Set(c._1, c._2)) - cave

  def size: Int = connections.size

  def allTripsPart1From(cave: Cave): Set[CaveTrip] = {
    def tripsRec(current: List[Cave], found: Set[CaveTrip] = Set.empty): Set[CaveTrip] = {
      val nexts = allConnectionsFor(current.last)
      val visitedSmalls = current.filter(_.isSmall).toSet
      val candidates = nexts.filter(c => c.isBig || !visitedSmalls.contains(c))
      if (candidates.isEmpty) found
      else {
        val newFound = if (candidates.exists(_ == end)) (found + CaveTrip(current :+ end)) else found
        candidates.flatMap(c => tripsRec(current :+ c, newFound))
      }
    }
    tripsRec(List(cave))
  }

  def allTripsPart2From(cave: Cave): Set[CaveTrip] = {
    def tripsRec(current: List[Cave], found: Set[CaveTrip] = Set.empty): Set[CaveTrip] = {
      val nexts = allConnectionsFor(current.last).filterNot(_ == Cave.start)
      val (visitedSmalls, twiceJokerBurned) = current.foldLeft((Set.empty[Cave], false)) { case ((visited, twice), cave) =>
        if (cave.isBig) (visited, twice)
        else (visited + cave, twice || visited.contains(cave))
      }
      val candidatesRequireJoker = nexts.map { n => if (n.isBig) n -> false else n -> visitedSmalls.contains(n) }.toMap
      val candidates = if (twiceJokerBurned) candidatesRequireJoker.filterNot(_._2).map(_._1).toSet else candidatesRequireJoker.keySet
      if (candidates.isEmpty || current.last == Cave.end) found
      else {
        val newFound = if (candidates.exists(_ == end)) (found + CaveTrip(current :+ end)) else found
        candidates.flatMap(c => tripsRec(current :+ c, newFound))
      }
    }
    tripsRec(List(cave))
  }
}


object Parser {
  import cats.parse.{Parser0, Parser => P, Numbers}
  import cats.parse.Rfc5234.{alpha, digit, sp, lf}

  val cave: P[Cave] = alpha.rep.map(l => Cave(l.toList.mkString))
  val line: P[(Cave, Cave)] = (cave <* P.char('-')) ~ cave
  val caveSystem: P[CaveSystem] = (line <* lf).rep.map(caves => CaveSystem(caves.toList.toSet))
}

@main def part1 = Input.parse(Parser.caveSystem) { caveSystem =>
  val trips = caveSystem.allTripsPart1From(Cave.start)
  println(s"There are ${trips.size} paths")
}

@main def part2 = Input.parse(Parser.caveSystem) { caveSystem =>
  val trips = caveSystem.allTripsPart2From(Cave.start)
  println(s"There are ${trips.size} paths")
}