import munit.FunSuite
import scala.collection.immutable.BitSet
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

class Day05Spec extends FunSuite {

    val example = """0,9 -> 5,9
                    |8,0 -> 0,8
                    |9,4 -> 3,4
                    |2,2 -> 2,1
                    |7,0 -> 7,4
                    |6,4 -> 2,0
                    |0,9 -> 2,9
                    |3,4 -> 1,4
                    |0,0 -> 8,8
                    |5,5 -> 8,2""".stripMargin

    test("A single line should be parseable") {
        checkParseFunc(NearbyVentsParser.line, "0,9 -> 5,9") { line =>
            assertEquals(line.start, Position(0,9))
            assertEquals(line.end, Position(5,9))
        }
    }

    test("Positions for vertical lines are correct") {
        val checked = Line(Position(3,4), Position(1,4)).allPositions()
        assertEquals(checked, Set(Position(1,4), Position(2,4), Position(3,4)))
    }

    test("Positions for horizontal lines are correct") {
        val checked = Line(Position(0,9), Position(2,9)).allPositions()
        assertEquals(checked, Set(Position(0,9), Position(1,9), Position(2,9)))
    }

    test("Exemplary field count") {
        val list = List(Line(Position(0,9), Position(5,9)),
                        Line(Position(0,9), Position(2,9)))
        val counts = list.fieldCounts()
        assertEquals(counts.count(_._2 == 1), 3)
        assertEquals(counts.count(_._2 == 2), 3)
    }

    test("Example input should be parseable") {
        checkParseFunc(NearbyVentsParser.lines, example) { lines =>
            assertEquals(lines.size, 10)
        }
    }

    test("Example input should contain 6 straight lines") {
        checkParseFunc(NearbyVentsParser.lines, example) { lines =>
            assertEquals(lines.filter(_.isStraight).size, 6)
        }
    }

    test("Example input should contain 5 dangerous places from straight lines") {
        checkParseFunc(NearbyVentsParser.lines, example) { lines =>
            val straightLines = lines.filter(_.isStraight)
            val dangerousFields = straightLines.fieldCounts().filter(_._2 > 1)
            assertEquals(dangerousFields.size, 5)
        }
    }

    test("Example input should only contain straight or diagonal lines") {
        checkParseFunc(NearbyVentsParser.lines, example) { lines =>
            lines.toList.foreach { line =>
                assert(line.isStraight || line.isDiagonal, s"Line $line is neither")
            }
        }
    }

    val diagonalExampleLine1 = Line(Position(1,1), Position(3,3))
    val diagonalExampleLine2 = Line(Position(9,7), Position(7,9))
    test("Positions for diagonal lines example 1 are correct") {
        assertEquals(diagonalExampleLine1.allPositions(), Set(
            Position(1,1), Position(2,2), Position(3,3)
        ))
    }

    test("Positions for diagonal lines example 2 are correct") {
        println(s"Unit vector is ${diagonalExampleLine2.unitVector} for $diagonalExampleLine2")
        assertEquals(diagonalExampleLine2.allPositions(), Set(
            Position(9,7), Position(8,8), Position(7,9)
        ))
    }

    test("Example input should contain 5 dangerous places from all lines") {
        checkParseFunc(NearbyVentsParser.lines, example) { lines =>
            val dangerousFields = lines.toList.fieldCounts().filter(_._2 > 1)
            assertEquals(dangerousFields.size, 12)
        }
    }

}