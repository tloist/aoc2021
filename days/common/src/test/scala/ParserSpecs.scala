import cats.parse.Parser
import munit.Assertions._

object ParserSpecs {
  
  def checkParseFunc[T](parser: Parser[T], input: String)(spec: T => Unit) = {
    parser.parseAll(input) match {
      case Right(result) => spec(result)
      case Left(Parser.Error(offset, expectations)) =>
        val failedInput = if (offset > 0) { s"""'${input.charAt(offset - 1)}'""" } else { "The very first character" }
        fail(s"""|Parsing of input '$input' failed at position: $offset!
                 |Input that failed: $failedInput.
                 |Expected: $expectations}
                 |""".stripMargin
        )
    }
  }
}
