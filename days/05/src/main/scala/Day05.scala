import cats._
import cats.data._
import cats.implicits._
import cats.data.NonEmptyCollection
import Line.*

case class Line(start: Position, end: Position) {
  assert(isStraight || isDiagonal)

  def isVertical: Boolean = start.x == end.x
  def isHorizontal: Boolean = start.y == end.y
  def isStraight: Boolean = isVertical || isHorizontal
  def isDiagonal: Boolean = Math.abs(start.x - end.x) == Math.abs(start.y - end.y)

  def unitVector: (Int, Int) = {
    val nonUnified = (end.x - start.x, end.y - start.y)
    val factor = Math.max(Math.abs(nonUnified._1), Math.abs(nonUnified._2)) // because it is diagonal, it is the same factor for both axes
    (nonUnified._1 / factor, nonUnified._2 / factor)
  }

  def allPositions(): Set[Position] = {
    val unit = unitVector
    LazyList.iterate(start)(_.addVector.tupled.apply(unit)).takeWhile(_ != end).appended(end).toSet
  }

  override def toString = this match {
    case _ if isHorizontal => s"Horizontal[$start -> $end]"
    case _ if isVertical => s"Vertical[$start -> $end]"
    case _ if isDiagonal => s"Diagonal[$start -> $end]"
    case _ => throw IllegalArgumentException(s"$start -> $end is neither horizontal, vertical nor diagonally")
  }
}


object NearbyVentsParser {
  import cats.parse.{Parser0, Parser => P, Numbers}
  import cats.parse.Rfc5234.{alpha, digit, sp, lf}

  val number: P[Int] = Numbers.digits.map(_.toInt)
  val arrow: P[Unit] = P.string("->").void
  val position: P[Position] = ((number <* P.char(',')) ~ number).map(Position.apply)
  val line: P[Line] = ((position <* (arrow surroundedBy sp)) ~ position).map(Line.apply)
  val lines: P[NonEmptyList[Line]] = (line <* lf.?).rep
}

case class Position(x: Int, y: Int) {
  def addVector(mx: Int, my: Int) = Position(x + mx, y + my)
}

extension (lines: List[Line])
  def fieldCounts(): Map[Position, Int] = lines.foldLeft(Map.empty[Position, Int]) { (map, line) =>
    line.allPositions().foldLeft(map) { (res, pos) =>
      res.updated(pos, res.getOrElse(pos, 0) + 1)
    }
  }


@main def part1(): Unit = NearbyVentsParser.lines.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right((remaining, lines)) => 
    val fieldCounts = lines.toList.filter(_.isStraight).fieldCounts()
    val count = fieldCounts.count(_._2 > 1)
    println(s"Dangerous positions: $count")
}

@main def part2(): Unit = NearbyVentsParser.lines.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right((remaining, lines)) => 
    val fieldCounts = lines.toList.fieldCounts()
    val count = fieldCounts.count(_._2 > 1)
    println(s"Dangerous positions: $count")
}
