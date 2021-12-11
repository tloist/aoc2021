import munit.FunSuite
import cats.*
import cats.data.*
import cats.implicits.*
import cats.parse.Parser as P
import Parser.Result.*
import ParserSpecs.checkParseFunc
import cats.parse.Parser.Expectation.InRange

class Day10Spec extends FunSuite {
  isValid("A pair of round brackets is valid","()")
  isValid("A pair of square brackets is valid", "[]")
  isValid("More complex, but valid #1", "([])")
  isValid("More complex, but valid #2", "{()()()}")
  isValid("More complex, but valid #3", "<([{}])>")
  isValid("More complex, but valid #4", "[<>({}){}[([])<>]]")
  isValid("More complex, but valid #5", "(((((((((())))))))))")

  isCorrupted("Corrupted example #1", "(]", ')')
  isCorrupted("Corrupted example #2", "{()()()>", '}')
  isCorrupted("Corrupted example #3", "(((()))}", ')')
  isCorrupted("Corrupted example #4", "<([]){()}[{}])", '>')

  isIncomplete("Example - Incomplete #1", "[({(<(())[]>[[{[]{<()<>>", "}}]])})]")
  isIncomplete("Example - Incomplete #2", "[(()[<>])]({[<{<<[]>>(", ")}>]})")
  isIncomplete("Example - Incomplete #3", "(((({<>}<{<{<>}{[]{[]{}", "}}>}>))))")
  isIncomplete("Example - Incomplete #4", "{<[[]]>}<{[{[{[]{()[[[]", "]]}}]}]}>")
  isIncomplete("Example - Incomplete #5", "<{([{{}}[<[[[<>{}]]]>[]]", "])}>")

  test("Example - Corrupted #1") {
    assertEquals(Parser.parse("{([(<{}[<>[]}>{[]{[(<()>"), Corrupted('}', ']'))
  }
  test("Example - Corrupted #2") {
    assertEquals(Parser.parse("[[<[([]))<([[{}[[()]]]"), Corrupted(')', ']'))
  }
  test("Example - Corrupted #3") {
    assertEquals(Parser.parse("[{[{({}]{}}([{[{{{}}([]"), Corrupted(']', ')'))
  }
  test("Example - Corrupted #4") {
    assertEquals(Parser.parse("[<(<(<(<{}))><([]([]()"), Corrupted(')', '>'))
  }
  test("Example - Corrupted #5") {
    assertEquals(Parser.parse("<{([([[(<>()){}]>(<<{{"), Corrupted('>', ']'))
  }



  val example =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]
      |""".stripMargin

  test("Part 1 example scoring is 26397") {
    assertEquals(part1Scoring(example.split("\n").map(Parser.parse)*), 26397)
  }

  test("Part 2 example scores") {
    val scores = part2Scoring(example.split("\n").map(Parser.parse)*)
    assertEquals(scores("}}]])})]"), BigInt(288957))
    assertEquals(scores(")}>]})"), BigInt(5566))
    assertEquals(scores("}}>}>))))"), BigInt(1480781))
    assertEquals(scores("]]}}]}]}>"), BigInt(995444))
    assertEquals(scores("])}>"), BigInt(294))
    assertEquals(scores.size, 5)
  }

  test("Part 2 example winning score") {
    val scores = part2Scoring(example.split("\n").map(Parser.parse)*)
    assertEquals(scores.toList.sortBy(_._2).apply(scores.size / 2)._2, BigInt(288957))
  }

  def isValid(name: String, input: String) = test(name) {
    assertEquals(Parser.parse(input), Legal)
  }
  def isCorrupted(name: String, input: String, expects: Char): Unit = test(name) {
    Parser.parse(input) match {
      case Corrupted(_, `expects`) =>
      case other => fail(s"Resulted is not corrupted but $other")
    }
  }
  def isIncomplete(name: String, input: String, expects: String): Unit = test(name) {
    assertEquals(Parser.parse(input), Incomplete(expects))
  }
}
