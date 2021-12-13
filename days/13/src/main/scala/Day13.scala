import cats._
import cats.data._
import cats.implicits._

case class Position(x: Int, y: Int)

enum Fold:
  case Horizontal(y: Int)
  case Vertical(x: Int)

case class Field(dots: Set[Position]) {
  def twoDim: String = (0 to dots.map(_.y).max).map { y =>
    (0 to dots.map(_.x).max).map { x =>
      if (dots.contains(Position(x,y))) "#" else "."
    }.mkString + "\n"
  }.mkString

  def fold(along: Fold): Field = along match {
    case Fold.Horizontal(y) =>
      assert(dots.forall(_.y != y), s"Constraint violated for fold on $along: " +
        "'dots will never appear exactly on a fold line'")
      Field(dots.map { dot => if (dot.y < y) dot else Position(dot.x, dot.y - 2*(dot.y - y)) })
    case Fold.Vertical(x) =>
      assert(dots.forall(_.x != x), s"Constraint violated for fold on $along: " +
        "'dots will never appear exactly on a fold line'")
      Field(dots.map { dot => if (dot.x < x) dot else Position(dot.x - 2*(dot.x - x), dot.y) })
  }

  def dotCount: Int = dots.size
}

object Parser {
  import cats.parse.{Parser0, Parser => P, Numbers}
  import cats.parse.Rfc5234.{alpha, digit, sp, lf}

  val position: P[Position] = ((Numbers.digits <* P.char(',')) ~ Numbers.digits)
    .map(t => Position(t._1.toInt, t._2.toInt))

  val field: P[Field] = (position <* lf.?).rep.map(l => Field(l.toList.toSet.toSet))

  val fold: P[Fold] = {
    val vertical = (P.char('x') *> P.char('=') *> Numbers.digits).map(no => Fold.Vertical(no.toInt))
    val horizontal = (P.char('y') *> P.char('=') *> Numbers.digits).map(no => Fold.Horizontal(no.toInt))
    P.string("fold along ") *> (vertical | horizontal)
  }

  val folds: P[NonEmptyList[Fold]] = (fold <* lf.?).rep

  val file: P[(Field, NonEmptyList[Fold])] = (field ~ (lf *> folds))
}

@main def part1 = Input.parse(Parser.file) { (field, folds) =>
  val afterFirst = field.fold(folds.head)
  println(s"Dots after first fold: ${afterFirst.dotCount}")
}

@main def part2 = Input.parse(Parser.file) { (field, folds) =>
  val result = folds.foldLeft(field) { _ fold _}
  println("Resulting code:")
  println
  println(result.twoDim)
}