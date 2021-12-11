import munit.FunSuite
import Field.*

class Day11Spec extends FunSuite {
  val ringExample = """11111
                      |19991
                      |19191
                      |19991
                      |11111""".stripMargin
  val ringField: Field = Field.from(ringExample)

  val example =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin
  val exampleField = Field.from(example)

  test("Small example after Step 1: 9 tiles flashed") {
    assertEquals(Field.from(ringExample).step()._1, 9)
  }

  test("Small example after Step 1: Board is as expected") {
    assertEquals(Field.from(ringExample).step()._2, Field.from(
      """34543
        |40004
        |50005
        |40004
        |34543""".stripMargin))
  }

  test("Field after 10 steps") {
    assertEquals(exampleField.nextFields(10), Field.from(
      """0481112976
        |0031112009
        |0041112504
        |0081111406
        |0099111306
        |0093511233
        |0442361130
        |5532252350
        |0532250600
        |0032240000""".stripMargin))
  }

  test("Flashes after 10 steps") {
    assertEquals(exampleField.nextEnlightments.take(10).sum, 204)
  }

  test("Field after 100 steps") {
    assertEquals(exampleField.nextFields(100), Field.from(
      """0397666866
        |0749766918
        |0053976933
        |0004297822
        |0004229892
        |0053222877
        |0532222966
        |9322228966
        |7922286866
        |6789998766""".stripMargin))
  }

  test("Flashes after 100 steps") {
    assertEquals(exampleField.nextEnlightments.take(100).sum, 1656)
  }

  test("Step 2: Synchronizing example is correct") {
    val sync = exampleField.nextEnlightments.zipWithIndex.find(_._1 == 100).map(_._2).get
    assertEquals(sync, 194)
  }

  def pos(x: Int, y: Int) = Position(x,y)

}
