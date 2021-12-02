import munit.FunSuite
import SubmarineCommand.*

class Day02bSpec extends FunSuite {

    test("Example first step") {
        val res = new SubmarinePart2().follows(Forward(5))
        assertEquals(res.horizontal, BigInt(5))
        assertEquals(res.aim, BigInt(0))
        assertEquals(res.depth, BigInt(0))
    }

    test("Example first two step") {
        val res = new SubmarinePart2().follows(Forward(5), Down(5))
        assertEquals(res.horizontal, BigInt(5))
        assertEquals(res.aim, BigInt(5))
        assertEquals(res.depth, BigInt(0))
    }

    test("Example first three step") {
        val res = new SubmarinePart2().follows(Forward(5), Down(5), Forward(8))
        assertEquals(res.horizontal, BigInt(13))
        assertEquals(res.aim, BigInt(5))
        assertEquals(res.depth, BigInt(40))
    }

    test("Example first four step") {
        val res = new SubmarinePart2().follows(Forward(5), Down(5), Forward(8), Up(3))
        assertEquals(res.horizontal, BigInt(13))
        assertEquals(res.aim, BigInt(2))
        assertEquals(res.depth, BigInt(40))
    }

    test("Example first five step") {
        val res = new SubmarinePart2().follows(Forward(5), Down(5), Forward(8), Up(3), Down(8))
        assertEquals(res.horizontal, BigInt(13))
        assertEquals(res.aim, BigInt(10))
        assertEquals(res.depth, BigInt(40))
    }

    test("Example final sizth step") {
        val res = new SubmarinePart2().follows(Forward(5), Down(5), Forward(8), Up(3), Down(8), Forward(2))
        assertEquals(res.horizontal, BigInt(15))
        assertEquals(res.aim, BigInt(10))
        assertEquals(res.depth, BigInt(60))
    }

}