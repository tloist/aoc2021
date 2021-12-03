import cats._
import cats.data._
import cats.implicits._
import cats.parse.{Parser0, Parser => P, Numbers}
import scala.collection.immutable.BitSet
import Rating.*
import cats.data.NonEmptyCollection

object BitSetParser {
  // Scala BitSet store the binary positions of bits that are true
  // Positions: x_n x_{n-1}, … , x_2, x_1, x_0
  // e.g. 010110 means positions 0 is not set, 1 is set, 2 is set and 4 is set => BitSet(1,2,4)
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val pZero: P[Boolean] = P.char('0').map(_ => false)
  val pOne: P[Boolean]  = P.char('1').map(_ => true)
  val pBit: P[Boolean]  = (pZero | pOne)
  val pBitSet: P[BitSet] = pBit.rep.map { bits => 
    val positivePositions = bits.reverse.zipWithIndex.toList.filter(_._1).map(_._2)
    positivePositions.foldLeft(BitSet.empty) { _ + _ }
  }
  val pBitSets: P[NonEmptyList[BitSet]] = pBitSet.repSep(whitespace)

  def parse(content: String): NonEmptyList[BitSet] = pBitSets.parse(content).map(_._2).getOrElse(
    throw new IllegalArgumentException(s"Error while parsing input")
  )
}


extension (set: BitSet)
  def toBinaryString: String = (set.max to 0 by -1).foldLeft("") { (res, pos) =>
    res + (if (set.contains(pos)) "1" else "0")
  }
  def toBinaryStringPaddedTo(length: Int): String = toBinaryString.reverse.padTo(length, '0').reverse

  def toDecimal: BigInt = (0 to set.max).foldLeft((0, 1)) { case ((res, incr), pos) =>
    if (set.contains(pos)) (res + incr, incr * 2) else (res, incr * 2)
  }._1

  def toInt: Int = toDecimal.toInt

  def reverse(totalLength: Int): BitSet = BitSet.fromSpecific((0 to totalLength).toSet diff set)

enum Rating:
  case OxygenGenerator
  case CO2Scrubber

extension (sets: NonEmptyList[BitSet])
  def highestBitPos: Int = sets.map(_.max).maximum 

  def gammaRate: BitSet = {
    val truePositionCount = sets.foldLeft(Map.empty[Int, Int]) { (counts, set) =>
      set.foldLeft(counts) { (res, pos) => res.updated(pos, res.getOrElse(pos, 0) + 1) }
    }
    truePositionCount.find(_._2 * 2 == sets.size).map(_._1).foreach { pos =>
      throw new IllegalStateException(
        s"Bit count on position $pos is equally split into half\n" +
         "Can't determine a most common bit for it")
    }
    val mostCommonBits = truePositionCount.collect { case (pos, count) if count > sets.size / 2 => pos }
    BitSet.fromSpecific(mostCommonBits)
  }

  def epsilonRate: BitSet = gammaRate.reverse(sets.highestBitPos)

  def gammaMultEpsilon: BigInt = gammaRate.toDecimal * epsilonRate.toDecimal

// These algorithms are not guaranteed to run a NonEmptyList
// e.g. if all BitSets have a bit set on position x and we search the C02 Scrubber, we remove all entries?!
extension (sets: List[BitSet])
  def highestBitPos: Int = sets.map(_.max).max 

  def unifyOnPosition(pos: Int, rating: Rating): List[BitSet] = 
    if (sets.size == 1) sets
    else {
      val oneCount = sets.count(_.contains(pos))
      val zeroCount = sets.size - oneCount
      val seekingBit = rating match {
        case OxygenGenerator => oneCount >= zeroCount
        case CO2Scrubber => oneCount < zeroCount
      }
      if (seekingBit) { sets.filter(_.contains(pos)) }
      else { sets.filter(!_.contains(pos)) }
    }

  def computeRating(rating: Rating): BitSet = {
    val result = (highestBitPos to 0 by -1).foldLeft(sets) { (remaining, pos) => remaining.unifyOnPosition(pos, rating) }
    result.head // if there are multiple, these should be identical - dedup here
  }

@main def part1(): Unit = {
  val bitSets = BitSetParser.parse(Input.asString())
  println(s"Gamma Rate:   ${bitSets.gammaRate.toBinaryStringPaddedTo(12)} -- ${bitSets.gammaRate.toDecimal}")
  println(s"Epsilon Rate: ${bitSets.epsilonRate.toBinaryStringPaddedTo(12)} -- ${bitSets.epsilonRate.toDecimal}")
  println(s"Final result: ${bitSets.gammaMultEpsilon}")
}

@main def part2(): Unit = {
  val bitSets = BitSetParser.parse(Input.asString()).toList
  val oxyRating = bitSets.computeRating(OxygenGenerator)
  val co2Rating = bitSets.computeRating(CO2Scrubber)
  println(s"OxyRating:   ${oxyRating.toBinaryStringPaddedTo(12)} -- ${oxyRating.toDecimal}")
  println(s"CO2 Rating: ${co2Rating.toBinaryStringPaddedTo(12)} -- ${co2Rating.toDecimal}")
  println(s"Final result: ${oxyRating.toDecimal * co2Rating.toDecimal}")
}