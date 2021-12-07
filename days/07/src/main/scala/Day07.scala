import cats._
import cats.data._
import cats.implicits._
import cats.data.NonEmptyCollection
import scala.annotation.tailrec
import CrabPositions.*

object CrabPositions {
  import cats.parse.{Parser => P, Numbers}

  val parser: P[NonEmptyList[Int]] = Numbers.digits.map(_.toInt).repSep(P.char(','))
  def parse(input: String) = parser.parse(input)

  extension (list: NonEmptyList[Int])
    def costForAligningAt(pos: Int, distance: (Int, Int) => Int): Int = list.foldLeft(0) { (sum, cur) => sum + distance(cur, pos)}
    def medianPos: Int = list.sorted.get((list.size - 1) / 2).get
    def posToAlignmentCost(distance: (Int, Int) => Int): Map[Int, Int] = 
      (list.minimum to list.maximum).map(k => (k, costForAligningAt(k, distance))).toMap
}

def part1Distance(start: Int, end: Int): Int = Math.abs(start - end)

@main def part1(): Unit = CrabPositions.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right(_, crabPos) =>
    val (pos, cost) = crabPos.posToAlignmentCost(part1Distance).minBy(_._2)
    println(s"Position $pos has cost $cost")
    val median = crabPos.medianPos
    println(s"Median is $median and has cost ${crabPos.costForAligningAt(median, part1Distance)}")
}

def part2Distance(start: Int, end: Int): Int = {
  val simple = Math.abs(start - end)
  (simple * simple + simple) / 2
}

@main def part2(): Unit = CrabPositions.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right(_, crabPos) =>
    val (pos, cost) = crabPos.posToAlignmentCost(part2Distance).minBy(_._2)
    println(s"Position $pos has cost $cost")
    val median = crabPos.medianPos
    println(s"Median is $median and has cost ${crabPos.costForAligningAt(median, part1Distance)}")
}


