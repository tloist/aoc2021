import cats._
import cats.data._
import cats.implicits._
import cats.parse.{Parser0, Parser => P, Numbers}
import SubmarineCommand.*

enum SubmarineCommand:
  case Forward(distance: Int)
  case Down(distance: Int)
  case Up(distance: Int)

object SubmarineCommand {
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val forwardParser: P[Forward] = (P.string("forward") *> whitespace.rep(1) *> Numbers.nonNegativeIntString).map(str => Forward(str.toInt))
  private val downParser: P[Down] = (P.string("down") *> whitespace.rep(1) *> Numbers.nonNegativeIntString).map(str => Down(str.toInt))
  private val upParser: P[Up] = (P.string("up") *> whitespace.rep(1) *> Numbers.nonNegativeIntString).map(str => Up(str.toInt))
  private val commandsParser: P[NonEmptyList[SubmarineCommand]] = (forwardParser | downParser | upParser).repSep(whitespace)

  def parse(content: String): NonEmptyList[SubmarineCommand] = commandsParser.parse(content).map(_._2).getOrElse(
    throw new IllegalArgumentException(s"Error while parsing input")
  )
}

case class SubmarinePart1(depth: Int = 0, horizontal: Int = 0) {
  def follow(instruction: SubmarineCommand): SubmarinePart1 = instruction match {
    case Forward(distance) => copy(horizontal = this.horizontal + distance)
    case Down(distance) => copy(depth = this.depth + distance)
    case Up(distance) => copy(depth = this.depth - distance)
  }

  override def toString: String = s"Submarine[depth:$depth | horizontal:$horizontal]"
}

@main def part1(): Unit = {
  val commands = SubmarineCommand.parse(Input.asString())
  val res = commands.foldLeft(new SubmarinePart1()) { _ follow _ }
  println(s"Resulting Submarine: $res - ${res.depth * res.horizontal}")
}

case class SubmarinePart2(depth: BigInt = 0, horizontal: BigInt = 0, aim: BigInt = 0) {
  def follow(instruction: SubmarineCommand): SubmarinePart2 = instruction match {
    case Forward(distance) => copy(
      horizontal = this.horizontal + distance,
      depth = this.depth + distance * this.aim
    )
    case Down(distance) => copy(aim = this.aim + distance)
    case Up(distance) => copy(aim = this.aim - distance)
  }

  def follows(instructions: SubmarineCommand*): SubmarinePart2 =
    instructions.foldLeft(this) { _ follow _}
}

@main def part2(): Unit = {
  val commands = SubmarineCommand.parse(Input.asString())
  val res = new SubmarinePart2().follows(commands.toList*)
  println(s"Resulting Submarine: $res >> ${res.depth * res.horizontal}")
}