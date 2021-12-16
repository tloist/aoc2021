import cats.*
import cats.data.*
import cats.implicits.*
import cats.parse.Rfc5234.wsp

type PairInsertionRules = Map[(Char, Char), Char]
type CharPair = (Char, Char)
type CharPairCounts = Map[CharPair, Long]


extension [K, N: Numeric](map: Map[K, N]) {
  def inc(key: K): Map[K,N] = inc(key, summon[Numeric[N]].one)
  def inc(key: K, increment: N)(using no: Numeric[N]): Map[K, N] =
    map.updated(key, no.plus(map.getOrElse(key, no.zero), increment))
}

case class Counts(counts: Map[CharPair, Long], leftMost: Char, rightMost: Char) {

  def step(pairInsertionRules: PairInsertionRules): Counts = Counts(
    pairInsertionRules.foldLeft(counts) { (res, rule) =>
      // Important: Read from original 'counts' because all rules are applied on the original input simultanously
      // Otherwise we would apply the rules from left to right (foldLeft) and rule 2 would operate on rule 1s output
      val count = counts.getOrElse(rule._1, 0L)
      if (count == 0) res
      else res.inc(rule._1, -count)
        .inc((rule._1._1, rule._2), count)
        .inc((rule._2, rule._1._2), count)
    }.filterNot(_._2 == 0), leftMost, rightMost)

  def steps(pairInsertionRules: PairInsertionRules): LazyList[Counts] =
    LazyList.iterate(this){ _ step pairInsertionRules }

  def characterCount: Map[Char, Long] = counts.foldLeft(Map.empty[Char, Long]) { (res, entry) =>
    val (pair, count) = entry
    res.inc(pair._1, count).inc(pair._2, count)
  }.inc(leftMost).inc(rightMost).mapValues(_ / 2).toMap // All characters are contained in 2 pairs except…

  def length: Long = characterCount.map(_._2).sum

  def printed: String = {
    val pairs = counts.filter(_._2 > 0).toList.sorted
      .map(t => f"  Pair ${t._1} is contained ${t._2}%3d times").mkString("\n")
    s"$leftMost…$rightMost with pairs: \n$pairs"
  }

  private def withoutEmptyCounts(): Counts = copy(counts = this.counts.filterNot(_._2 == 0))
}

object Counts {
  def readFromString(input: String): Counts = {
    require(input.length >= 2, s"Invalid lenght: A pair needs at least 2 characters")
    val pairs = input.sliding(2)
      .map(str => (str(0), str(1)))
      .foldLeft(Map.empty[CharPair, Long])(_ inc _)
    Counts(pairs, input.head, input.last)
  }
}



object Parser {
  import cats.parse.{Parser0, Parser => P, Numbers}
  import cats.parse.Rfc5234.{alpha, digit, sp, lf}

  val charPairCounts: P[Counts] = alpha.rep.string.map(Counts.readFromString)
  val pairInsertionRule: P[PairInsertionRules] = ((alpha ~ alpha) ~ (P.string("->").surroundedBy(wsp) *> alpha))
    .map(t => Map(t._1 -> t._2))
  val pairInsertionRules: P[PairInsertionRules] = (pairInsertionRule <* lf.?).rep.map {rules =>
    rules.foldLeft(Map.empty) { (res, mapping) =>
      require(res.keySet.intersect(mapping.keySet).isEmpty)
      res ++ mapping
    }
  }
  val input: P[(Counts, PairInsertionRules)] = ((charPairCounts <* lf.rep(2, 2)) ~ pairInsertionRules)
}

@main def part1 = Input.parse(Parser.input) { (init, rules) =>
  val counts = init.steps(rules)(10).characterCount.map(_._2)
  val result = counts.max - counts.min
  println(s"The expected count is: $result")
}

@main def part2 = Input.parse(Parser.input) { (init, rules) =>
  val counts = init.steps(rules)(40).characterCount.map(_._2)
  val result = counts.max - counts.min
  println(s"The expected count is: $result")
}
