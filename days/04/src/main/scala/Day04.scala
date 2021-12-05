import cats._
import cats.data._
import cats.implicits._
import cats.parse.{Parser0, Parser => P, Numbers}
import cats.data.NonEmptyCollection
import BingoBoard.Position

object BingoBoard {
  case class Position(x: BigInt, y: BigInt)
  enum WinOption {
    case Row(no: BigInt)
    case Column(no: BigInt)

    def positions(diameter: BigInt): Set[Position] = this match {
      case Row(no) => (BigInt(0) until diameter).map(Position(_, no)).toSet
      case Column(no) => (BigInt(0) until diameter).map(Position(no, _)).toSet
    }

    override def toString = this match {
      case Row(no) => s"Row[$no]"
      case Column(no) => s"Column[$no]"
    }

    def isRealizedBy(diameter: BigInt, drawn: Set[Position]): Boolean = {
      val affected = positions(diameter)
      (affected intersect drawn) == affected
    }
  }

  def allRowsFor(diameter: BigInt): Set[WinOption] = (BigInt(0) until diameter).map(WinOption.Row.apply).toSet
    def allColumnFor(diameter: BigInt): Set[WinOption] = (BigInt(0) until diameter).map(WinOption.Column.apply).toSet
    def allWinOptionsFor(diameter: BigInt): Set[WinOption] = allRowsFor(diameter) union allColumnFor(diameter)
}

case class BingoBoard(diameter: Int, board: Map[Position, BigInt]) {
  assert(board.size == diameter * diameter)
  import BingoBoard.*
  val reversed: Map[BigInt, Set[Position]] = board.view.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
  def size = board.size

  def isWonBy(drawn: Iterable[BigInt]): Boolean = {
    val markedPos = markedPositions(drawn)
    allWinOptionsFor(diameter).exists(_.isRealizedBy(diameter, markedPos))
  }

  def markedPositions(drawn: Iterable[BigInt]): Set[Position] = drawn.flatMap(no => reversed.getOrElse(no, Set.empty)).toSet
  def unmarkedPositions(drawn: Iterable[BigInt]): Set[Position] = board.keySet diff markedPositions(drawn)
}

case class BingoGame(players: Map[Int, BingoBoard], drawn: List[BigInt] = List.empty, won: Set[Int] = Set.empty, wonNumber: Option[BigInt] = None) {
  
  def draw(no: BigInt): BingoGame =
    if (won.nonEmpty) this
    else {
      val futureDrawn = drawn :+ no
      val futureWon = players.filter(_._2.isWonBy(futureDrawn)).keySet
      copy(
        drawn = futureDrawn,
        won = futureWon,
        wonNumber = if (futureWon.nonEmpty) Option(no) else None
      )
    }
  
  def markedPositionsFor(playerNo: Int): Set[Position] =
    players.get(playerNo).map(_.markedPositions(drawn)).getOrElse(Set.empty)

  def score(): Option[BigInt] = wonNumber.map { wonNo =>
    val sumOfUnmarkedFields = won.map(players).map { board =>
      board.unmarkedPositions(drawn).map(board.board).sum
    }.max
    sumOfUnmarkedFields * wonNo
  }
}

object BingoGame {
  def init(boards: Iterable[BingoBoard]) = BingoGame(boards.zipWithIndex.map(x => (x._2, x._1)).toList.toMap)
}

object BingoParser {
  val whitespace: P[Unit] = P.charIn(" \t\r").void
  val newline: P[Unit] = P.charIn("\n").void
  val comma: P[Unit] = P.char(',').void

  val inputLine: P[NonEmptyList[BigInt]] = Numbers.bigInt.repSep(comma)
  val boardLine: P[NonEmptyList[BigInt]] = (whitespace.rep0.with1 *> Numbers.bigInt).rep
  val board: P[BingoBoard] = (boardLine.repSep(newline) <* newline.?).map { grid =>
    val rowCount = grid.size
    grid.find(_.size != rowCount).foreach { invalidRow =>
      throw new IllegalArgumentException(s"Invalid row with ${invalidRow.size} numbers, where $rowCount would have been expected!\n" +
        s"Row: ${invalidRow.toList.mkString(", ")}")}
    BingoBoard(rowCount, (for {
      (row, y) <- grid.zipWithIndex.toList
      (no, x) <- row.zipWithIndex.toList
    } yield Position(x,y) -> no).toMap)
  }
  val boards: P[NonEmptyList[BingoBoard]] = board.repSep(newline)

  val totalInput = (inputLine <* newline.rep) ~ boards
}

@main def part1(): Unit = BingoParser.totalInput.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right((remaining, (inputs, boards))) => 
    val result = inputs.foldLeft(BingoGame.init(boards.toList)) { _ draw _ }
    println(s"Winning number: ${result.wonNumber}")
    println(s"Winning score: ${result.score()}")
    println(s"Winning board: ${result.won}")
}

case class SquidBingoGame(players: Map[Int, BingoBoard],
  drawn: List[BigInt] = List.empty,
  won: Set[Int] = Set.empty,
  lastWonNumber: Option[BigInt] = None,
  lastWonPlayer: Set[Int] = Set.empty) {
  
  def draw(no: BigInt): SquidBingoGame =
    if (lastWonPlayer.nonEmpty) this
    else {
      val futureDrawn = drawn :+ no
      val futureWon = players.filter(_._2.isWonBy(futureDrawn)).keySet
      val futureLastWonPlayer = if (players.keySet == futureWon) players.keySet diff won else Set.empty
      copy(
        drawn = futureDrawn,
        won = futureWon,
        lastWonNumber = if (futureLastWonPlayer.nonEmpty) Some(no) else None,
        lastWonPlayer = futureLastWonPlayer
      )
    }
  
  def markedPositionsFor(playerNo: Int): Set[Position] =
    players.get(playerNo).map(_.markedPositions(drawn)).getOrElse(Set.empty)


  def score(): Option[BigInt] = lastWonNumber.map { lastWonNo =>
    val sumOfUnmarkedFields = lastWonPlayer.map { lastWonPlayerNo => 
      val board = players(lastWonPlayerNo)
      board.unmarkedPositions(drawn).map(board.board).sum
    }.min
    sumOfUnmarkedFields * lastWonNo
  }
}

object SquidBingoGame {
  def init(boards: Iterable[BingoBoard]) = SquidBingoGame(boards.zipWithIndex.map(x => (x._2, x._1)).toList.toMap)
}

@main def part2(): Unit = BingoParser.totalInput.parse(Input.asString()) match {
  case Left(error) => throw new IllegalArgumentException(s"Error while parsing\n$error")
  case Right((remaining, (inputs, boards))) => 
    val result = inputs.foldLeft(SquidBingoGame.init(boards.toList)) { _ draw _ }
    println(s"Last winning number: ${result.lastWonNumber}")
    println(s"Last winning score: ${result.score()}")
    println(s"Last winning board: ${result.lastWonPlayer}")
}