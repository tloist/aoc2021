import munit.FunSuite
import scala.collection.immutable.BitSet
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc
import Rating.*

extension (sc: StringContext)
  // Poor mans version of string interpolation, sufficient for these tests
  def b(args: Any*): BitSet = {
    if (args.size != 0) throw new IllegalArgumentException("Expected no arguments!")
    BitSetParser.pBitSets.parse(sc.parts.mkString) match {
      case Right(res) => res._2.head
      case Left(error) => throw new IllegalArgumentException(s"Error parsing: $error")
    }
  }

class Day03Spec extends FunSuite {

    test("Can parse a single line") {
      val bitSet = BitSetParser.parse("10110").head
      assertEquals(bitSet.size, 3)
      assert(bitSet.contains(1))
      assert(bitSet.contains(2))
      assert(bitSet.contains(4))
    }

    test("Can parse multiple lines") {
      val bitSets = BitSetParser.parse("10110\n00011".stripMargin).toList
      assertEquals(bitSets.size, 2)
      assert(bitSets.contains(BitSet(1,2,4)))
      assert(bitSets.contains(BitSet(0, 1)))
    }

    test("Parsing of example input") {
      checkParseFunc(BitSetParser.pBitSets, """00100
                                              |11110
                                              |10110
                                              |10111
                                              |10101
                                              |01111
                                              |00111
                                              |11100
                                              |10000
                                              |11001
                                              |00010
                                              |01010""".stripMargin) { bitSets => 
        assertEquals(bitSets.size, 12)
      }
    }

    val exampleBitsets = NonEmptyList.of(b"00100",
                                         b"11110",
                                         b"10110",
                                         b"10111",
                                         b"10101",
                                         b"01111",
                                         b"00111",
                                         b"11100",
                                         b"10000",
                                         b"11001",
                                         b"00010",
                                         b"01010")
    
    test("Example calculation of gammaRate") {
      assertEquals(exampleBitsets.gammaRate, b"10110")
      assertEquals(exampleBitsets.gammaRate.toInt, 22)
    }

    test("Example calculation of epsilonRate") {
      assertEquals(exampleBitsets.epsilonRate, b"01001")
      assertEquals(exampleBitsets.epsilonRate.toInt, 9)
    }

    test("Example final A result") {
      assertEquals(exampleBitsets.gammaMultEpsilon.toInt, 198)
    }

    test("Check if the most common bit in step #1 runs for oxygen") {
      val result = exampleBitsets.toList.unifyOnPosition(4, OxygenGenerator)
      assertEquals(result.size, 7)
      List(b"11110", b"10110", b"10111", b"10101", b"11100", b"10000", b"11001").foreach { exp =>
        assert(result.contains(exp))
      }
    }

    test("Check if the most common bit in step #2 runs for oxygen") {
      val previous = List(b"11110",
                          b"10110",
                          b"10111",
                          b"10101",
                          b"11100",
                          b"10000",
                          b"11001")
      val result = previous.unifyOnPosition(3, OxygenGenerator)
      assertEquals(result.size, 4)
      assert(result.contains(b"10110"))
      assert(result.contains(b"10111"))
      assert(result.contains(b"10101"))
      assert(result.contains(b"10000"))
    }

    test("Check if the most common bit in step #3 runs for oxygen") {
      val previous = List(b"10110",
                          b"10111",
                          b"10101",
                          b"10000")
      val result = previous.unifyOnPosition(2, OxygenGenerator)
      assertEquals(result.size, 3)
      assert(result.contains(b"10110"))
      assert(result.contains(b"10111"))
      assert(result.contains(b"10101"))
    }

    test("Check if the most common bit in step #4 runs for oxygen") {
      val previous = List(b"10110",
                          b"10111",
                          b"10101")
      val result = previous.unifyOnPosition(1, OxygenGenerator)
      assertEquals(result.size, 2)
      assert(result.contains(b"10110"))
      assert(result.contains(b"10111"))
    }

    test("Check if the most common bit in step #5 runs for oxygen") {
      val previous = List(b"10110", 
                          b"10111")
      val result = previous.unifyOnPosition(0, OxygenGenerator)
      assertEquals(result.size, 1)
      assert(result.contains(b"10111"))
    }

    test("Check if the most common bit in step #2 runs for CO2 scrubber") {
      val previous = List(b"00100", 
                          b"01111",
                          b"00111",
                          b"00010",
                          b"01010")
      val result = previous.unifyOnPosition(3, CO2Scrubber)
      assertEquals(result.size, 2)
      assert(result.contains(b"01111"))
      assert(result.contains(b"01010"))
    }

    test("Check if the most common bit in step #3 runs for CO2 scrubber") {
      val previous = List(b"01111",
                          b"01010")
      val result = previous.unifyOnPosition(2, CO2Scrubber)
      assertEquals(result.size, 1)
      assert(result.contains(b"01010"))
    }

    test("Check if the most common bit in step #4 runs for CO2 scrubber, if it just needs to skip") {
      val previous = List(b"01010")
      val result = previous.unifyOnPosition(1, CO2Scrubber)
      assertEquals(result.size, 1)
      assert(result.contains(b"01010"))
    }

    test("Check if computing the Oxygen Generator Rating works") {
      val oxyRating = exampleBitsets.toList.computeRating(OxygenGenerator)
      assertEquals(oxyRating, b"10111")
      assertEquals(oxyRating.toInt, 23)
    }

    test("Check if computing the CO2 Scrubber Rating works") {
      val oxyRating = exampleBitsets.toList.computeRating(CO2Scrubber)
      assertEquals(oxyRating, b"01010")
      assertEquals(oxyRating.toInt, 10)
    }

}