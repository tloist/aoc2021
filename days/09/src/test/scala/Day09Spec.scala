import munit.FunSuite
import cats.*
import cats.data.*
import cats.implicits.*

class Day09Spec extends FunSuite {
  val exampleMap =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin
  val exampleField = Field.from(exampleMap)

  test("Example field identifies the correct low points") {
    assertEquals(exampleField.lowPoints(), Set(
      Position(1,0), Position(9,0), Position(2, 2), Position(6, 4)
    ))
  }

  test("The sum of all risk levels is 15") {
    assertEquals(exampleField.lowPointsScore(), 15)
  }

  test("Part2: Basin top-left") {
    assertEquals(exampleField.expandBasin(Position(1,0)),
      Set(Position(0,1), Position(0,0), Position(1,0)))
  }

  test("Part2: Basin top-right") {
    assertEquals(exampleField.expandBasin(Position(9,0)), Set(
      Position(9,0), Position(8,0), Position(7,0), Position(6,0), Position(5,0),
      Position(6,1), Position(8,1), Position(9,1),
      Position(9,2)
    ))
  }

  test("Part2: Identify all basins sizes") {
    val basinSizes = exampleField.lowPoints().toList.map(exampleField.expandBasin)
    assertEquals(basinSizes.sortBy(-_.size).take(3).map(_.size).product, 1134)
  }
}
