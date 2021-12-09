import better.files._
import java.io.InputStream
import cats.parse.Parser

object Input {
  def asStream(part: TaskPart = TaskPart.First): InputStream = Resource.getAsStream(part.inputFileName)
  def asString(part: TaskPart = TaskPart.First): String = Resource.getAsString(part.inputFileName)
  def asLines(part: TaskPart = TaskPart.First): List[String] = asString(part).split('\n').toList

  def parse[T, R](parser: Parser[T], part: TaskPart = TaskPart.First)(body: T => R): R = parser.parse(asString(part)) match {
    case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
    case scala.util.Right(remaining, input) => body(input)
  }

}
