import munit.FunSuite
import scala.collection.immutable.BitSet
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc
import BingoBoard.*

class Day04Spec extends FunSuite {

    test("Parser: Example inputline can be parsed") {
        val inputLine = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
        checkParseFunc(BingoParser.inputLine, inputLine) { numbers =>
            assertEquals(numbers.size, 27)
            assertEquals(numbers.head.toInt, 7)
            assertEquals(numbers.last.toInt, 1)
        }
    }

    val board1 = """22 13 17 11  0
                   | 8  2 23  4 24
                   |21  9 14 16  7
                   | 6 10  3 18  5
                   | 1 12 20 15 19""".stripMargin

    val board2 = """ 3 15  0  2 22
                   | 9 18 13 17  5
                   |19  8  7 25 23
                   |20 11 10 24  4
                   |14 21 16 12  6""".stripMargin

    val board3 = """14 21 17 24  4
                   |10 16 15  9 19
                   |18  8 23 26 20
                   |22 11 13  6  5
                   | 2  0 12  3  7""".stripMargin

    val boards = s"$board1\n\n$board2\n\n$board3"

    test("Parser: First BingoBoard 1 can be parsed") {
        checkParseFunc(BingoParser.board, board1) { board =>
            assertEquals(board.diameter.toInt, 5)
            assertEquals(board.size.toInt, 25)
        }
    }

    test("Parser: First BingoBoard 2 can be parsed") {
        checkParseFunc(BingoParser.board, board2) { board =>
            assertEquals(board.diameter.toInt, 5)
            assertEquals(board.size.toInt, 25)
        }
    }

    test("Parser: All BingoBoards can be parsed together") {
        checkParseFunc(BingoParser.boards, boards) { boards =>
            assertEquals(boards.size, 3)
            boards.toList.foreach { board =>
                assertEquals(board.diameter, 5)
            }
        }
    }

    def pos(x: Int, y: Int): Position = Position(BigInt(x), BigInt(y))

    test("Example after first five numbers") {
        val inputs = List(7,4,9,5,11).map(BigInt.apply)
        checkParseFunc(BingoParser.boards, boards) { boards =>
            val game = BingoGame(boards.zipWithIndex.map(x => (x._2, x._1)).toList.toMap)
            val result = inputs.foldLeft(game) { _ draw _ }

            assertEquals(result.markedPositionsFor(0), Set(
                pos(3,0),
                pos(3,1),
                pos(1, 2), pos(4,2),
                pos(4,3)
            ))
            assertEquals(result.markedPositionsFor(1), Set(
                pos(0,1), pos(4,1),
                pos(2,2),
                pos(1,3), pos(4,3)
            ))
            assertEquals(result.markedPositionsFor(2), Set(
                pos(4,0),
                pos(3,1),
                pos(1,3), pos(4,3),
                pos(4,4)
            ))
        }
    }

    test("Example after first eleven numbers") {
        val inputs = List(7,4,9,5,11,17,23,2,0,14,21).map(BigInt.apply)
        checkParseFunc(BingoParser.boards, boards) { boards =>
            val game = BingoGame(boards.zipWithIndex.map(x => (x._2, x._1)).toList.toMap)
            val result = inputs.foldLeft(game) { _ draw _ }

            assertEquals(result.markedPositionsFor(0), Set(
                pos(2,0), pos(3,0), pos(4,0),
                pos(1,1), pos(2,1), pos(3,1),
                pos(0,2), pos(1,2), pos(2,2), pos(4,2),
                pos(4,3)
            ))
            assertEquals(result.markedPositionsFor(1), Set(
                pos(2,0), pos(3,0),
                pos(0,1), pos(3,1), pos(4,1),
                pos(2,2), pos(4,2),
                pos(1,3), pos(4,3),
                pos(0,4), pos(1, 4)
            ))
            assertEquals(result.markedPositionsFor(2), Set(
                pos(0,0), pos(1,0), pos(2,0), pos(4,0),
                pos(3,1),
                pos(2,2),
                pos(1,3), pos(4,3),
                pos(0,4), pos(1,4), pos(4,4)
            ))
        }
    }

    test("Third board is won after twelth drawn number") {
        val drawn = List(7,4,9,5,11,17,23,2,0,14,21,24).map(BigInt.apply)
        checkParseFunc(BingoParser.board, board3) { board =>
            assertEquals(board.markedPositions(drawn), Set(
                pos(0,0), pos(1,0), pos(2,0), pos(3,0), pos(4,0),
                pos(3,1),
                pos(2,2),
                pos(1,3), pos(4,3),
                pos(0,4), pos(1,4), pos(4,4)
            ))
            assert(board.isWonBy(drawn))
        }
    }

    test("Example after the twelth number") {
        val inputs = List(7,4,9,5,11,17,23,2,0,14,21,24).map(BigInt.apply)
        checkParseFunc(BingoParser.boards, boards) { boards =>
            val game = BingoGame(boards.zipWithIndex.map(x => (x._2, x._1)).toList.toMap)
            val result = inputs.foldLeft(game) { _ draw _ }

            assertEquals(result.markedPositionsFor(0), Set(
                pos(2,0), pos(3,0), pos(4,0),
                pos(1,1), pos(2,1), pos(3,1), pos(4,1),
                pos(0,2), pos(1,2), pos(2,2), pos(4,2),
                pos(4,3)
            ))
            assertEquals(result.markedPositionsFor(1), Set(
                pos(2,0), pos(3,0),
                pos(0,1), pos(3,1), pos(4,1),
                pos(2,2), pos(4,2),
                pos(1,3), pos(3,3), pos(4,3),
                pos(0,4), pos(1, 4)
            ))
            assertEquals(result.markedPositionsFor(2), Set(
                pos(0,0), pos(1,0), pos(2,0), pos(3,0), pos(4,0),
                pos(3,1),
                pos(2,2),
                pos(1,3), pos(4,3),
                pos(0,4), pos(1,4), pos(4,4)
            ))

            assertEquals(result.won, Set(2))
            assertEquals(result.wonNumber, Option(BigInt(24)))
            assertEquals(result.score(), Option(BigInt(4512)))
        }
    }

    test("Example: Drawing more numbers do not change the outcome") {
        val inputs = List(7,4,9,5,11,17,23,2,0,14,21,24,10).map(BigInt.apply)
        checkParseFunc(BingoParser.boards, boards) { boards =>
            val result = inputs.foldLeft(BingoGame.init(boards.toList)) { _ draw _ }

            assertEquals(result.markedPositionsFor(0), Set(
                pos(2,0), pos(3,0), pos(4,0),
                pos(1,1), pos(2,1), pos(3,1), pos(4,1),
                pos(0,2), pos(1,2), pos(2,2), pos(4,2),
                pos(4,3)
            ))
            assertEquals(result.markedPositionsFor(1), Set(
                pos(2,0), pos(3,0),
                pos(0,1), pos(3,1), pos(4,1),
                pos(2,2), pos(4,2),
                pos(1,3), pos(3,3), pos(4,3),
                pos(0,4), pos(1, 4)
            ))
            assertEquals(result.markedPositionsFor(2), Set(
                pos(0,0), pos(1,0), pos(2,0), pos(3,0), pos(4,0),
                pos(3,1),
                pos(2,2),
                pos(1,3), pos(4,3),
                pos(0,4), pos(1,4), pos(4,4)
            ))

            assertEquals(result.won, Set(2))
            assertEquals(result.wonNumber, Option(BigInt(24)))
            assertEquals(result.score(), Option(BigInt(4512)))
        }
    }

    test("Example: Last one winning") {
        val inputs = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1).map(BigInt.apply)
        checkParseFunc(BingoParser.boards, boards) { boards =>
            val result = inputs.foldLeft(SquidBingoGame.init(boards.toList)) { _ draw _ }
            assertEquals(result.lastWonPlayer.map(_.toInt), Set(1))
            assertEquals(result.lastWonNumber.map(_.toInt), Some(13))
            assertEquals(result.score().map(_.toInt), Some(1924))
        }
    }

}