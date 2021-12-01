import better.files._
import java.io.InputStream

object Input {
  def asStream(part: TaskPart = TaskPart.First): InputStream = Resource.getAsStream(part.inputFileName)
  def asString(part: TaskPart = TaskPart.First): String = Resource.getAsString(part.inputFileName)
  def asLines(part: TaskPart = TaskPart.First): List[String] = asString(part).split('\n').toList
}
