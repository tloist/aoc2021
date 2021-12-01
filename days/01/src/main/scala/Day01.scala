import cats._
import cats.data._
import cats.implicits._

def numbers() = Input.asLines(TaskPart.First).map(_.toInt)

def amountOfDepthIncreases(depths: Iterable[Int]): Int = depths.foldLeft((Option.empty[Int], 0)) { case ((previous, count), current) => 
    previous match {
      case None => (Some(current), count)
      case Some(number) if current > number => (Some(current), count + 1)
      case _ => (Some(current), count)
    }
  }._2

@main def part1(): Unit = {
  val increases = amountOfDepthIncreases(numbers())
  println(s"The number of increases is: $increases")
}

@main def part2(): Unit = {
  val increases = amountOfDepthIncreases(numbers().sliding(3).map(_.sum).toList)
  println(s"The number of increases in sliding window of 3 is: $increases")
}