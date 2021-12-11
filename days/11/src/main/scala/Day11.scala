import Field.*
import scala.annotation.tailrec

case class Field(board: Map[Position, Int]) {
  def neighbour(pos: Position): Set[Position] = diagonalAndStraightNeighbour.map(pos plus _).filter(board.contains)

  def step(): (Int, Field) = {
    @tailrec def flashes(alreadyFlashed: Set[Position], boardRec: Map[Position, Int]): (Int, Field) =
      val flashed = boardRec.filter(_._2 > 9).map(_._1).filterNot(alreadyFlashed.contains)
      if (flashed.isEmpty) (alreadyFlashed.size, Field(boardRec))
      else {
        val newFlashed = alreadyFlashed ++ flashed
        val afterFlashes = flashed.foldLeft(boardRec) { (res, pos) =>
          neighbour(pos).foldLeft(res) { (board, neighbourPos) =>
            if (newFlashed.contains(neighbourPos)) board
            else board.updated(neighbourPos, board(neighbourPos) + 1)
          }
        }
        flashes(newFlashed, newFlashed.foldLeft(afterFlashes) { (res, pos) => res.updated(pos, 0) })
      }
    flashes(Set.empty, board.view.mapValues(_ + 1).toMap)
  }

  def nextEnlightments = LazyList.unfold(this) { s => Option(s.step()) }
  def nextFields = this #:: LazyList.unfold(this) { s =>
    val newField = s.step()._2
    Some((newField, newField))
  }

  def twoDimensional: String = {
    (0 to board.keys.map(_.y).max).map { y =>
      (0 to board.keys.map(_.x).max).map { x =>
        board.get(Position(x,y)).map(v => if (v >= 10) "+" else v.toString).getOrElse("-")
      }.mkString + "\n"
    }.mkString
  }
}

object Field {
  def from(content: String): Field = Field((for {
      (line, y) <- content.linesIterator.zipWithIndex
      (height, x) <- line.zipWithIndex
    } yield Position(x,y) -> height.asDigit
  ).toMap)

  case class Position(x: Int, y: Int) {
    def plus(vector: (Int, Int)): Position = Position(x + vector._1, y + vector._2)
  }

  val diagonalAndStraightNeighbour = Set(
    (-1, -1), (0, -1), (1, -1),
    (-1,  0),          (1,  0),
    (-1,  1), ( 0, 1), (1,  1)
  )
  val straightNeighbour = Set( (1, 0), (0, 1), (-1, 0), (0, -1) )
}

@main def part1 = {
  val flashesAfter100 = Field.from(Input.asString()).nextEnlightments.take(100).sum
  println(s"Flashes after 100 steps: $flashesAfter100")
}

@main def part2 = {
  val synchronizingPoint = Field.from(Input.asString()).nextEnlightments.zipWithIndex.find(_._1 == 100).map(_._2).get
  println(s"All octopuses flash synchronizes at ${synchronizingPoint + 1}") // 0-based
}