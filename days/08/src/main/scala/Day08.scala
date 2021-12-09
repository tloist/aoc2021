import cats._
import cats.data._
import cats.implicits._
import cats.data.NonEmptyCollection

enum Digit {
  case _0, _1, _2, _3, _4, _5, _6, _7, _8, _9
  def toInt: Int = ordinal
  override def toString = toInt.toString
}

enum Segment {
  case A, B, C, D, E, F, G
}

val regularDigitToSegment: Map[Digit, Set[Segment]] = {
  import Digit.*
  import Segment.*
  Map(
    _0 -> Set(A, B, C, E, F, G),
    _1 -> Set(C, F),
    _2 -> Set(A, C, D, E, G),
    _3 -> Set(A, C, D, F, G),
    _4 -> Set(B, C, D, F),
    _5 -> Set(A, B, D, F, G),
    _6 -> Set(A, B, D, E, F, G),
    _7 -> Set(A, C, F),
    _8 -> Set(A, B, C, D, E, F, G),
    _9 -> Set(A, B, C, D, F, G)
  )
}

object Segment {
  import cats.parse.{Parser0, Parser => P, Numbers}
  given Order[Segment] = Order.from { (a, b) => a.ordinal - b.ordinal }
  given Show[Segment] = Show.fromToString
  val pSegA: P[Segment] = P.char('a').map(_ => Segment.A)
  val pSegB: P[Segment] = P.char('b').map(_ => Segment.B)
  val pSegC: P[Segment] = P.char('c').map(_ => Segment.C)
  val pSegD: P[Segment] = P.char('d').map(_ => Segment.D)
  val pSegE: P[Segment] = P.char('e').map(_ => Segment.E)
  val pSegF: P[Segment] = P.char('f').map(_ => Segment.F)
  val pSegG: P[Segment] = P.char('g').map(_ => Segment.G)
  val pSegment: P[Segment] = pSegA | pSegB | pSegC | pSegD | pSegE | pSegF | pSegG
  val pSegments: P[NonEmptySet[Segment]] = pSegment.rep.map(_.toNes)
}

object Parsers {
  import cats.parse.{Parser0, Parser => P, Numbers}
  import cats.parse.Rfc5234.{alpha, digit, sp, lf}

  opaque type MultipleDigits = NonEmptyList[NonEmptySet[Segment]]
  given Show[MultipleDigits] = Show.show { _.map(_.toList.sorted.mkString("[", ",", "]")).mkString_("Segments(", ", ", ")") }
  def digitsFrom(head: NonEmptySet[Segment], tail: NonEmptySet[Segment]*): MultipleDigits = NonEmptyList.of(head, tail*)
  extension (digits: MultipleDigits)
    def lidSegmentCounts: NonEmptyList[Int] = digits.map(_.size.toInt)
    def deduceSegmentMapping(): Map[NonEmptySet[Segment], Digit] = {
      assert(digits.size == 10)
      val segm1 = digits.find(_.size == 2).get
      val segm4 = digits.find(_.size == 4).get
      val segm7 = digits.find(_.size == 3).get
      val segm8 = digits.find(_.size == 7).get
      val sixSegments = digits.filter(_.size == 6)
      // 9 is the number with 6 digits which contains all of 4 digits
      val segm9 = sixSegments.find { c => segm4.forall(c.contains) }.get
      // Other than 9, 0 is the only digit that contains all of 1s digits
      val segm0 = sixSegments.filter(_ != segm9).find { c => segm1.forall(c.contains) }.get
      // 6 is the only digit left that contains 6 segments and is neither of the above
      val segm6 = sixSegments.filter(_ != segm9).filter(_ != segm0).head

      val fiveSegments = digits.filter(_.size == 5)
      // 5 is the only one, whose segments are all included in 6
      val segm5 = fiveSegments.find { _.forall(segm6.contains) }.get
      // 3 is the only digit that contains all of 1s digits
      val segm3 = fiveSegments.find { c => segm1.forall(c.contains) }.get
      // 2 is the only digit that contains 5 segments that is left
      val segm2 = fiveSegments.filter(_ != segm5).filter(_ != segm3).head
      import Digit.*
      Map(
        segm0 -> _0, segm1 -> _1, segm2 -> _2, segm3 -> _3, segm4 -> _4,
        segm5 -> _5, segm6 -> _6, segm7 -> _7, segm8 -> _8, segm9 -> _9
      )
    }
    def decode(code: Map[NonEmptySet[Segment], Digit]): NonEmptyList[Digit] = digits.map(code.apply)
    def decodeToText(code: Map[NonEmptySet[Segment], Digit]): String = digits.toList.map(code.apply).map(_.toInt).mkString
    def decodeToLong(code: Map[NonEmptySet[Segment], Digit]): Long = decodeToText(code).toLong
  


  case class Line(allDigits: MultipleDigits, fourOutputDigits: MultipleDigits)
  val pDelimiter: P[Unit] = P.char('|')
  val pMultipleDigits: P[MultipleDigits] = Segment.pSegments.repSep(sp)
  val pLine: P[Line] = ((pMultipleDigits <* pDelimiter.surroundedBy(sp)) ~ pMultipleDigits).map(Line.apply)
  val pLines: P[NonEmptyList[Line]] = (pLine <* lf.?).rep <* lf.?

  extension (line: Line)
    def deduceCode(): Long = line.fourOutputDigits.decodeToLong(line.allDigits.deduceSegmentMapping())
}

@main def part1(): Unit = Input.parse(Parsers.pLines) { lines =>
  val uniqueSegmentCounts = Set(2,3,4,7) // Digits, 1,7,4 and 8
  val count = lines.toList.map(_.fourOutputDigits.lidSegmentCounts.filter(uniqueSegmentCounts.contains(_)).size).sum
  println(s"The input contains $count simple identifiable digits")
}

@main def part2(): Unit = Input.parse(Parsers.pLines) { lines =>
  val sumOfAllLines = lines.toList.map(_.deduceCode()).sum
  println(s"All lines added up result in $sumOfAllLines")
}


