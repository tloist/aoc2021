import munit.FunSuite
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc
import CrabPositions.*

class Day07Spec extends FunSuite {
    val exampleList = NonEmptyList.of(16,1,2,0,4,2,7,1,2,14)

    test("Can parse example") {
        checkParseFunc(CrabPositions.parser, "16,1,2,0,4,2,7,1,2,14") { initialFishes =>
            assertEquals(initialFishes, exampleList)
        }
    }

    test("Cost of example alignment") {
        assertEquals(exampleList.costForAligningAt(1, part1Distance), 41)
        assertEquals(exampleList.costForAligningAt(2, part1Distance), 37)
        assertEquals(exampleList.costForAligningAt(3, part1Distance), 39)
    }

    test("Minimal cost from example") {
        val (pos, cost) = exampleList.posToAlignmentCost(part1Distance).minBy(_._2)
        assertEquals(pos, 2)
        assertEquals(cost, 37)
    }

    test("Part2: Cost of example alignment on position 2 with quadratic distance") {
        assertEquals(exampleList.costForAligningAt(2, part2Distance), 206)
    }

    test("Part2: Cost of move 16 -> 5 is 66") {
        assertEquals(part2Distance(16, 5), 66)
    }

    test("Part2: Cost of move 14 -> 5 is 45") {
        assertEquals(part2Distance(14, 5), 45)
    }

    test("Part2: Cost of move 4 -> 5 is 1") {
        assertEquals(part2Distance(4, 5), 1)
    }

    test("Part2: Cost of example alignment on position 2 with quadratic distance") {
        val (pos, cost) = exampleList.posToAlignmentCost(part2Distance).minBy(_._2)
        assertEquals(pos, 5)  
        assertEquals(cost, 168)  
    }
}