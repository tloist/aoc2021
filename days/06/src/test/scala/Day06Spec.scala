import munit.FunSuite
import cats._
import cats.data._
import cats.implicits._
import ParserSpecs.checkParseFunc

class Day06Spec extends FunSuite {
    import Lanternfish.*

    test("Can parse example") {
        checkParseFunc(Lanternfish.lanternfishParser, "3,4,3,1,2") { initialFishes =>
            assertEquals(initialFishes.size, 5)
        }
    }

    val example = Lanternfish.listOfAges(3,4,3,1,2)
    test("Example after 1 day") {
        val expected = Lanternfish.listOfAges(2,3,2,0,1)
        assertEquals(example.tick(), expected)
        assertEquals(example.afterDays(1), expected)
    }

    test("Example after 2 days") {
        assertEquals(example.afterDays(2), Lanternfish.listOfAges(1,2,1,6,0,8))
    }

    test("Example after 3 days") {
        assertEquals(example.afterDays(3), Lanternfish.listOfAges(0,1,0,5,6,7,8))
    }

    test("Example after 11 days") { 
        assertEquals(example.afterDays(11), Lanternfish.listOfAges(6,0,6,4,5,6,0,1,1,2,6,7,8,8,8))
    }

    test("Example after 18 days") {
        assertEquals(example.afterDays(18), Lanternfish.listOfAges(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8))
    }

    extension (ages: Map[Int, BigInt])
        def print(prefix: String = ""): Unit = {
            if (prefix.nonEmpty) println(prefix)
            val indent = if(prefix.nonEmpty) "\t" else ""
            ages.toList.sorted.foreach { (age, count) =>
                println(f"${indent}Age $age%1d: $count")
            }
            println(s"Total population: ${ages.totalPopulation}")
            println()
        }

    test("Part2: Example after 1 day #1") {
        val agesAfter1Day = example.toAges().tick()
        assertEquals(agesAfter1Day(0).toInt, 1)
        assertEquals(agesAfter1Day(1).toInt, 1)
        assertEquals(agesAfter1Day(2).toInt, 2)
        assertEquals(agesAfter1Day(3).toInt, 1)
        assertEquals(agesAfter1Day.totalPopulation, BigInt(5))

        (4 to 10).foreach { age =>
            assertEquals(agesAfter1Day.get(age), None)
        }
    }

    test("Part2: Example after 1 day #2") {
        val agesAfter1Day = example.toAges().afterDays(1)
        assertEquals(agesAfter1Day(0).toInt, 1)
        assertEquals(agesAfter1Day(1).toInt, 1)
        assertEquals(agesAfter1Day(2).toInt, 2)
        assertEquals(agesAfter1Day(3).toInt, 1)
        assertEquals(agesAfter1Day.totalPopulation, BigInt(5))
        (4 to 10).foreach { age =>
            assertEquals(agesAfter1Day.get(age), None)
        }
    }

    test("Part2: Newborn are equally birth ready after 2 days than newborn") {
        val after3Days = Map(
            0 -> BigInt(2),
            1 -> BigInt(1),
            5 -> BigInt(1),
            6 -> BigInt(1),
            7 -> BigInt(1),
            8 -> BigInt(1)
        )
        val after4Days = after3Days.tick()
        assertEquals(after4Days(6).toInt, 3)
    }

    test("Part2: Example after 18 day") {
        val agesAfter18Day = example.toAges().afterDays(18)
        agesAfter18Day.print()
        assertEquals(agesAfter18Day(0).toInt, 3)
        assertEquals(agesAfter18Day(1).toInt, 5)
        assertEquals(agesAfter18Day(2).toInt, 3)
        assertEquals(agesAfter18Day(3).toInt, 2)
        assertEquals(agesAfter18Day(4).toInt, 2)
        assertEquals(agesAfter18Day(5).toInt, 1)
        assertEquals(agesAfter18Day(6).toInt, 5)
        assertEquals(agesAfter18Day(7).toInt, 1)
        assertEquals(agesAfter18Day(8).toInt, 4)
        assertEquals(agesAfter18Day.totalPopulation, BigInt(26))
    }

    test("Part2: Example after 80 day") {
        assertEquals(example.totalPopulationAfter(80).toInt, 5934)
    }

    test("Part2: Example after 256 day") {
        assertEquals(example.totalPopulationAfter(256), BigInt(26984457539L))
    }

}