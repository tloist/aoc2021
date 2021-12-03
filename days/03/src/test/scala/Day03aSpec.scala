import munit.FunSuite
import scala.collection.immutable.BitSet
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

class Day03aSpec extends FunSuite {

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

    val exampleInput = """00100
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
                                 |01010""".stripMargin

    test("Example calculation of gammaRate") {
      checkParseFunc(BitSetParser.pBitSets, exampleInput) { bitSets => 
        assertEquals(bitSets.size, 12)
        assertEquals(bitSets.gammaRate.toBinaryStringPaddedTo(5), "10110")
        assertEquals(bitSets.gammaRate.toInt, 22)
      }
    }

    test("Example calculation of epsilonRate") {
      checkParseFunc(BitSetParser.pBitSets, exampleInput) { bitSets => 
        assertEquals(bitSets.epsilonRate.toBinaryStringPaddedTo(5), "01001")
        assertEquals(bitSets.epsilonRate.toInt, 9)
      }
    }

    test("Example final result") {
      checkParseFunc(BitSetParser.pBitSets, exampleInput) { bitSets => 
        assertEquals(bitSets.gammaMultEpsilon.toInt, 198)
      }
    }

}