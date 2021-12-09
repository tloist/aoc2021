import scala.annotation.tailrec

case class Position(x: Int, y: Int) {
  def plus(vector: (Int, Int)): Position = Position(x + vector._1, y + vector._2)
}
case class Field(board: Map[Position, Int]) {
  def neighbour(pos: Position): Set[Position] = Set( (1, 0), (0, 1), (-1, 0), (0, -1) )
    .map(pos plus _).filter(board.contains)

  def lowPoints(): Set[Position] = board.filter { (pos, height) => neighbour(pos).forall(board(_) > height) }.keySet
  def lowPointsScore(): Int = lowPoints().toList.map(1 + board(_)).sum

  def expandBasin(initial: Position): Set[Position] = {
    def basinNeighbour(pos: Position): Set[Position] = neighbour(pos)
      .filter { cand => board(cand) < 9 && board(pos) < board(cand) }
    @tailrec def expandBasin(candidates: Set[Position], basin: Set[Position]): Set[Position] =
      if (candidates.isEmpty) basin
      else {
        val nexts = basinNeighbour(candidates.head)
        expandBasin(candidates.tail ++ nexts, basin ++ nexts)
      }
    expandBasin(Set(initial), Set(initial))
  }
}
object Field {
  def from(content: String): Field = Field((for {
      (line, y) <- content.linesIterator.zipWithIndex
      (height, x) <- line.zipWithIndex
    } yield Position(x,y) -> height.asDigit
  ).toMap)
}

@main def part1 = {
  val lowPointScore = Field.from(Input.asString()).lowPointsScore()
  println(s"The risk sum is $lowPointScore")
}

@main def part2 = {
  val field = Field.from(Input.asString())
  val basins = field.lowPoints().toList.map(field.expandBasin)
  val top3Basins = basins.toList.sortBy(-_.size).take(3)
  val product = top3Basins.map(_.size).product
  println(s"The top3 basins have sizes: ${top3Basins.map(_.size).mkString(", ")}")
  println(s"Their product is $product")
}