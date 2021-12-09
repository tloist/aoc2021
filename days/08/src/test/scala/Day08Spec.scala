import munit.FunSuite
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

class Day08Spec extends FunSuite {
    val firstExampleLine = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    val firstLineSegments = {
        import Segment.*
        Parsers.digitsFrom(
            NonEmptySet.of(A, B, C, D, E, F, G),
            NonEmptySet.of(B, C, D, E, F),
            NonEmptySet.of(B, C, D, E, F, G),
            NonEmptySet.of(B, C, E, G)
        )
    }

    test("An input line can be parsed") {
        checkParseFunc(Parsers.pLine, firstExampleLine) { line =>
            assertEquals(line.fourOutputDigits, firstLineSegments)
        }
    }

    test("Multiple input lines with an ending newline can be parsed") {
        val input = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
                      #edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
                      #fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
                      #""".stripMargin('#')
        checkParseFunc(Parsers.pLines, input) { lines => 
            assertEquals(lines.size, 3)
        }
    }

    test("First example line lids the correct segment counts") {
        assertEquals(firstLineSegments.lidSegmentCounts, NonEmptyList.of(7, 5, 6, 4))
    }

    object Part2 {
        import Segment.*
        val allDigits = Parsers.digitsFrom(
            NonEmptySet.of(A, C, E, D, G, F, B),
            NonEmptySet.of(C, D, F, B, E),
            NonEmptySet.of(G, C, D, F, A),
            NonEmptySet.of(F, B, C, A, D),
            NonEmptySet.of(D, A, B),
            NonEmptySet.of(C, E, F, A, B, D),
            NonEmptySet.of(C, D, F, G, E, B),
            NonEmptySet.of(E, A, F, B),
            NonEmptySet.of(C, A, G, E, D, B),
            NonEmptySet.of(A, B),
        )
        val output = Parsers.digitsFrom(
            NonEmptySet.of(C, D, F, E, B),
            NonEmptySet.of(F, C, A, D, B),
            NonEmptySet.of(C, D, F, E, B),
            NonEmptySet.of(C, D, B, A, F)
        )

        val line = Parsers.Line(allDigits, output)
    }

    test("Part2: Deduced mapping for line is correct") {
        import Segment.*
        import Digit.*
        val deduced = Part2.allDigits.deduceSegmentMapping()
        assertEquals(deduced(NonEmptySet.of(C, A, G, E, D, B)), _0)
        assertEquals(deduced(NonEmptySet.of(A, B)), _1)
        assertEquals(deduced(NonEmptySet.of(G, C, D, F, A)), _2)
        assertEquals(deduced(NonEmptySet.of(F, B, C, A, D)), _3)
        assertEquals(deduced(NonEmptySet.of(E, A, F, B)), _4)
        assertEquals(deduced(NonEmptySet.of(C, D, F, B, E)), _5)
        assertEquals(deduced(NonEmptySet.of(C, D, F, G, E, B)), _6)
        assertEquals(deduced(NonEmptySet.of(D, A, B)), _7)
        assertEquals(deduced(NonEmptySet.of(A, C, E, D, G, F, B)), _8)
        assertEquals(deduced(NonEmptySet.of(C, E, F, A, B, D)), _9)
    }

    test("Part2: Deduced out is correct") {
        assertEquals(Part2.line.deduceCode(), 5353L)
    }
}
