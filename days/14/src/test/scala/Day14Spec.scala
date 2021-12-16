import munit.FunSuite
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

extension (str: String) {
  def characterCount: Map[Char, Long] = str.groupMapReduce(c => c)(_ => 1L)(_ + _)
}

class Day14Spec extends FunSuite {

  val exampleCounts = Counts(Map(
    ('N', 'N') -> 1L,
    ('N', 'C') -> 1L,
    ('C', 'B') -> 1L,
  ), 'N', 'B')

  val exampleInsertionRules = Map(
    ('C', 'H') -> 'B',
    ('H', 'H') -> 'N',
    ('C', 'B') -> 'H',
    ('N', 'H') -> 'C',
    ('H', 'B') -> 'C',
    ('H', 'C') -> 'B',
    ('H', 'N') -> 'C',
    ('N', 'N') -> 'C',
    ('B', 'H') -> 'H',
    ('N', 'C') -> 'B',
    ('N', 'B') -> 'B',
    ('B', 'N') -> 'B',
    ('B', 'B') -> 'N',
    ('B', 'C') -> 'B',
    ('C', 'C') -> 'N',
    ('C', 'N') -> 'C',
  )

  test("Parser can read template") {
    checkParseFunc(Parser.charPairCounts, "NNCB") { counts =>
      assertEquals(counts, exampleCounts)
      assertEquals(counts.characterCount, Map(
        'N' -> 2L,
        'C' -> 1L,
        'B' -> 1L,
      ))
    }
  }

  test("Parser can read pair insertion rules") {
    checkParseFunc(Parser.pairInsertionRules,
      """CH -> B
        |HH -> N
        |CB -> H
        |NH -> C
        |HB -> C
        |HC -> B
        |HN -> C
        |NN -> C
        |BH -> H
        |NC -> B
        |NB -> B
        |BN -> B
        |BB -> N
        |BC -> B
        |CC -> N
        |CN -> C""".stripMargin) { rules =>
      assertEquals(rules, exampleInsertionRules)
    }
  }

  test("Parser can read complete example") {
    val input =
      """NNCB
        |
        |CH -> B
        |HH -> N
        |CB -> H
        |NH -> C
        |HB -> C
        |HC -> B
        |HN -> C
        |NN -> C
        |BH -> H
        |NC -> B
        |NB -> B
        |BN -> B
        |BB -> N
        |BC -> B
        |CC -> N
        |CN -> C""".stripMargin
    checkParseFunc(Parser.input, input) { (counts, rules) =>
      assertEquals(counts, exampleCounts)
      assertEquals(rules, exampleInsertionRules)
    }
  }

  test("Example after step 1") {
    val res1 = exampleCounts.step(exampleInsertionRules)
    assertEquals(res1, Counts.readFromString("NCNBCHB"))
    val res2 = exampleCounts.steps(exampleInsertionRules)(1)
    assertEquals(res2, Counts.readFromString("NCNBCHB"))
    assertEquals(res2.characterCount, "NCNBCHB".characterCount)
  }

  test("Example after step 2") {
    val res = exampleCounts.steps(exampleInsertionRules)(2)
    assertEquals(res, Counts.readFromString("NBCCNBBBCBHCB"))
    assertEquals(res.characterCount, "NBCCNBBBCBHCB".characterCount)
  }

  test("Example after step 3") {
    val res = exampleCounts.steps(exampleInsertionRules)(3)
    assertEquals(res, Counts.readFromString("NBBBCNCCNBBNBNBBCHBHHBCHB"))
    assertEquals(res.characterCount, "NBBBCNCCNBBNBNBBCHBHHBCHB".characterCount)
  }

  test("Example after step 4") {
    val res = exampleCounts.steps(exampleInsertionRules)(4)
    assertEquals(res, Counts.readFromString("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"))
    assertEquals(res.characterCount, "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB".characterCount)
  }

  test("Example after step 5") {
    assertEquals(exampleCounts.steps(exampleInsertionRules)(5).length, 97L)
  }

  test("Example after step 10") {
    val res = exampleCounts.steps(exampleInsertionRules)(10)
    assertEquals(res.length, 3073L)
    assertEquals(res.characterCount, Map(
      'B' -> 1749L,
      'C' -> 298L,
      'H' -> 161L,
      'N' -> 865L,
    ))
  }

}