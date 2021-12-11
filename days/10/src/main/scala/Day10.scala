import cats.parse.Parser.Expectation.InRange
import cats.*
import cats.data.*
import cats.implicits.*
import cats.parse.Parser as P

object Parser {
  val brackets = P.recursive[Unit] { recursive =>
    val round = P.char('(').void <* recursive.? <* P.char(')')
    val square = P.char('[').void <* recursive.? <* P.char(']')
    val curly = P.char('{').void <* recursive.? <* P.char('}')
    val angle = P.char('<').void <* recursive.? <* P.char('>')
    P.oneOf(round :: square :: curly :: angle :: Nil).rep.void
  }

  enum Result:
    case Legal
    case Corrupted(actual: Char, expected: Char)
    case Incomplete(missing: String)

  def parse(input: String): Result = Parser.brackets.parse(input) match {
    case Right(_) => Result.Legal
    case Left(P.Error(offset, expected)) if offset == input.length && expected.size == 1 => expected.head match
      case InRange(`offset`, lower, upper) if lower == upper =>
        Result.Incomplete(autoComplete(input + lower).substring(input.length))
      case unknown => throw new IllegalStateException(s"Unknown parsing error encountered at offset $offset: $unknown")
    case Left(P.Error(offset, expected)) if offset == input.length =>
      throw new IllegalStateException(s"Ambiguous incomplete string found!")
    case Left(P.Error(offset, expected)) if expected.size == 1 => expected.head match {
      case InRange(`offset`, lower, upper) if lower == upper => Result.Corrupted(input(offset), lower)
      case unknown => throw new IllegalStateException(s"Unknown parsing error encountered at offset $offset: $unknown")
    }
    case unknown => throw new IllegalStateException(s"Unknown error while parsing '$input' encountered: $unknown")
  }

  def autoComplete(input: String): String = parse(input) match
    case Result.Legal => input
    case Result.Incomplete(c) => autoComplete(input + c)
    case Result.Corrupted(actual, expected) =>
      throw new IllegalArgumentException(s"Input '$input' is not auto completable because it is already corrupted!")
}

def part1Scoring(results: Parser.Result*): Int = results.collect {
  case Parser.Result.Corrupted(char, _) => char match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }
}.sum

@main def part1 = {
  val score = part1Scoring(Input.asLines().map(Parser.parse)*)
  println(s"The parsing score for corrupted lines is: $score")
}

def part2Scoring(results: Parser.Result*): Map[String, BigInt] = results.collect {
  case Parser.Result.Incomplete(missing) => missing -> missing.foldLeft(BigInt(0)) { (res, char) =>
    res * 5 + (char match
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    )
  }
}.toMap

@main def part2 = {
  val scores = part2Scoring(Input.asLines().map(Parser.parse)*).toList.sortBy(_._2)
  val (missing, score) = scores.toList.sortBy(_._2).apply(scores.size / 2)
  println(s"The winning score is $score for '$missing'")
}
