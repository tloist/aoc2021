import cats._
import cats.data._
import cats.implicits._
import cats.data.NonEmptyCollection
import scala.annotation.tailrec

object Lanternfish {
  import cats.parse.{Parser0, Parser => P, Numbers}
  import cats.parse.Rfc5234.{alpha, digit, sp, lf}
  opaque type Lanternfish = Int

  val lanternfishParser: P[NonEmptyList[Lanternfish]] = Numbers.digits.map(_.toInt).repSep(P.char(','))

  def listOfAges(age: Int, ages: Int*): NonEmptyList[Lanternfish] = NonEmptyList.of(age, ages*)

  extension (ages: Map[Int, BigInt])
    def tick(): Map[Int, BigInt] = {
      val parentCount = ages.getOrElse(0, BigInt(0))
      val aged = ages.map { (age, count) => (age - 1, count) }.filterKeys(_ >= 0).toMap
      if (parentCount == 0) aged
      else aged
        .updated(6, aged.getOrElse(6, BigInt(0)) + parentCount)
        .updated(8, aged.getOrElse(8, BigInt(0)) + parentCount)
    }

    @tailrec
    def afterDays(noOfDays: Int): Map[Int, BigInt] = {
      require(noOfDays >= 0)
      if (noOfDays == 0) ages
      else ages.tick().afterDays(noOfDays - 1)
    }

    def totalPopulation: BigInt = ages.values.sum

  extension (list: NonEmptyList[Lanternfish])
    // to match the example, we don't use flatMap here, which would be easier but would append the new born fishes 'in place'
    // instead we remember the newborn count and append them at the very end
    def tick(): NonEmptyList[Lanternfish] = {
      val withoutNewborn = list.map { fish => if (fish != 0) fish - 1 else 6 }
      val newborns = LazyList.continually(8).take(list.count(_ == 0).toInt).toList
      withoutNewborn.concat(newborns)
    }

    @tailrec
    def afterDays(noOfDays: Int): NonEmptyList[Lanternfish] = {
      require(noOfDays >= 0)
      if (noOfDays == 0) list
      else list.tick().afterDays(noOfDays - 1)
    }

    def toAges(): Map[Int, BigInt] = list.foldLeft(Map.empty[Int, BigInt]) { (ages, fish) =>
      ages.updated(fish, ages.getOrElse(fish, BigInt(0)) + BigInt(1))
    }

    def totalPopulationAfter(noOfDays: Int): BigInt =
      list.toAges().afterDays(noOfDays).totalPopulation
}

@main def part1(): Unit = Lanternfish.lanternfishParser.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right(_, fishes) =>
    val after80Days = fishes.afterDays(80)
    println(s"After 80 days there are ${after80Days.size} fishes present")
}

@main def part2(): Unit = Lanternfish.lanternfishParser.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right(_, fishes) =>
    val after256Days = fishes.totalPopulationAfter(256)
    println(s"After 80 days there are $after256Days fishes present")
}
