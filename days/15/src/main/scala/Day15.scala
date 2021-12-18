import cats.*
import cats.data.*
import cats.implicits.*

import scala.annotation.tailrec
import scala.collection.mutable

case class Position(x: Int, y: Int) {
  def addVector(vec: (Int, Int)): Position = Position(x + vec._1, y + vec._2)
}

case class Field(baseCosts: Map[Position, Int]) {
  val avgCost: BigDecimal = BigDecimal(baseCosts.values.sum) / baseCosts.size

  def twoDim: String = (0 to baseCosts.keys.map(_.y).max).map { y =>
    (0 to baseCosts.keys.map(_.x).max).map { x =>
      baseCosts.get(Position(x,y)).getOrElse("?")
    }.mkString + "\n"
  }.mkString

  def neighbours(pos: Position): Set[Position] = Set((-1, 0), (0, -1), (1, 0), (0, 1))
    .map(pos.addVector)
    .filter(baseCosts.contains)

  def aStar(from: Position, goal: Position): List[Position] = {
    // Estimation function: Manhattan distance to goal
    def estimate(pos: Position): BigDecimal = (Math.abs(goal.x - pos.x) + Math.abs(goal.y - pos.y))
    // Set of known reachable nodes
    val open = mutable.Set(from)
    // Currently best known path to this node
    val costBestKnownPath = mutable.Map(from -> BigDecimal(0))
    // Educated guess what the costs for a reachable node
    val estimatedCost = mutable.Map(from -> estimate(from))
    // For each node the predecessor on the best path
    val predecessorBestPath = mutable.Map.empty[Position, Position]


    def reconstruct(pos: Position): List[Position] =
      if pos == from then List()
      else pos +: reconstruct(predecessorBestPath(pos))

    while (open.nonEmpty) {
      val current = open.minBy(estimatedCost.apply) // not very efficient…
      if (current == goal) return reconstruct(current);
      open.remove(current)
      neighbours(current).foreach { next =>
        val nextCost = costBestKnownPath(current) + baseCosts(next)
        if (!costBestKnownPath.contains(next) || nextCost < costBestKnownPath(next)) {
          predecessorBestPath.update(next, current)
          costBestKnownPath.update(next, nextCost)
          estimatedCost.update(next, nextCost + estimate(next))
          if (!open.contains(next)) {
            open.add(next)
          }
        }
      }
    }
    return List.empty
  }

  def expand(x: Int, y: Int): Field = {
    require(x >= 1 && y >= 1)
    def value(x: Int): Int = LazyList.iterate(0)(i => (i % 9) + 1)(x)
    def fieldXY(expX: Int, expY: Int): Map[Position, Int] = (for {
      y <- 0 to 9
      x <- 0 to 9
    } yield {
      val base = baseCosts(Position(x, y))
      val newCost = value(base + expX + expY)
      Position(expX * 10 + x, expY * 10 + y) -> newCost
    }).toMap

    val expansions = ((0 to y).flatMap { expY =>
      (0 to x).flatMap { expX =>
        fieldXY(expX, expY)
      }
    }).toMap

    Field(baseCosts ++ expansions)
  }

  def costOfPath(path: Position*): Int = path.map(baseCosts.apply).sum

  def pathUpperLeftToLowerRight(): List[Position] = aStar(upperLeft, lowerRight)

  def upperLeft = baseCosts.keys.minBy(t => (t.x, t.y))
  def lowerRight = baseCosts.keys.maxBy(t => (t.x, t.y))

  def size: Int = baseCosts.size

  private def tenXtenField(offsetVec: (Int, Int) = (0,0)): Set[Position] =
    (for {
        baseY <- (0 to 9)
        baseX <- (0 to 9)
      } yield Position(baseX + offsetVec._1, baseY + offsetVec._2)
    ).toSet
}

object Field {
  def from(content: String): Field = Field((for {
      (line, y) <- content.linesIterator.zipWithIndex
      (height, x) <- line.zipWithIndex
    } yield Position(x,y) -> height.asDigit
  ).toMap)
}

@main def part1 = {
  val field = Field.from(Input.asString())
  val path = field.pathUpperLeftToLowerRight()
  val totalCosts = field.costOfPath(path*)
  println(s"Total cost of the optimum path is $totalCosts")
}

@main def part2 = {
  val field = Field.from(Input.asString()).expand(4, 4)
  val path = field.pathUpperLeftToLowerRight()
  val totalCosts = field.costOfPath(path*)
  println(s"Total cost of the optimum path is $totalCosts")
  println
  println(path)
}