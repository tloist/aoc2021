import munit.FunSuite
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

class Day13Spec extends FunSuite {

  val exampleFolds = List(Fold.Horizontal(7), Fold.Vertical(5))
  val exampleField = Field(Set(
    Position(6,10), Position(0,14), Position(9,10), Position(0,3), Position(10,4), Position(4,11), Position(6,0),
    Position(6,12), Position(4,1), Position(0,13), Position(10,12), Position(3,4), Position(3,0), Position(8,4),
    Position(1,10), Position(2,14), Position(8,10), Position(9,0)
  ))

  test("Can read positions") {
    checkParseFunc(Parser.field,
      """6,10
        |0,14
        |9,10
        |0,3
        |10,4
        |4,11
        |6,0
        |6,12
        |4,1
        |0,13
        |10,12
        |3,4
        |3,0
        |8,4
        |1,10
        |2,14
        |8,10
        |9,0""".stripMargin) { field =>
      assertEquals(field, exampleField)
    }
  }

  test("Can parse folds") {
    checkParseFunc(Parser.folds,
      """fold along y=7
        |fold along x=5""".stripMargin) { folds =>
      assertEquals(folds.toList, exampleFolds)
    }
  }

  test("Can parse complete example input") {
    checkParseFunc(Parser.file,
      """6,10
        |0,14
        |9,10
        |0,3
        |10,4
        |4,11
        |6,0
        |6,12
        |4,1
        |0,13
        |10,12
        |3,4
        |3,0
        |8,4
        |1,10
        |2,14
        |8,10
        |9,0
        |
        |fold along y=7
        |fold along x=5""".stripMargin) { (field, folds) =>
      assertEquals(field, exampleField)
      assertEquals(folds.toList, exampleFolds)
    }
  }

  def fieldFrom(content: String): Field = Field((for {
    (line, y) <- content.split("\n").zipWithIndex
    (char, x) <- line.zipWithIndex
  } yield char match {
    case '#' => Option(Position(x, y))
    case '.' => None
    case unknown => throw new IllegalArgumentException(s"Unexpected character '$unknown' while reading field")
  }).toSet.flatten)

  test("Example field is parsed correctly") {
    assertEquals(exampleField, fieldFrom(
      """...#..#..#.
        |....#......
        |...........
        |#..........
        |...#....#.#
        |...........
        |...........
        |...........
        |...........
        |...........
        |.#....#.##.
        |....#......
        |......#...#
        |#..........
        |#.#........""".stripMargin))
  }

  test("Example after first fold is correct") {
    assertEquals(exampleField.fold(Fold.Horizontal(7)), fieldFrom(
      """#.##..#..#.
        |#...#......
        |......#...#
        |#...#......
        |.#.#..#.###
        |...........
        |...........""".stripMargin))
  }

  test("Example from first to second fold is correct") {
    val afterFirst = fieldFrom(
      """#.##..#..#.
        |#...#......
        |......#...#
        |#...#......
        |.#.#..#.###
        |...........
        |...........""".stripMargin)
    assertEquals(afterFirst.fold(Fold.Vertical(5)), fieldFrom(
      """#####
        |#...#
        |#...#
        |#...#
        |#####""".stripMargin))
  }

  test("Dot count after first fold") {
    assertEquals(exampleField.fold(exampleFolds.head).dotCount, 17)
  }

}
