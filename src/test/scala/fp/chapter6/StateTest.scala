package fp.chapter6

import org.scalatest.FunSuite

class StateTest extends FunSuite {


  test("unit") {
    val instance = State.unit[Int, Int](1)
    val (a, s) = instance.run(2)

    assert(a === 1)
    assert(s === 2)
  }

  test("map") {
    val instance = State[Int, Int](i => (i*2, i*3)).map(i => i.toString)
    val (a, s) = instance.run(1)
    assert(a === "2")
    assert(s === 3)
  }

  test("map2") {
    val x = State[Int, Int](i => (i*3, i*4))
    val instance = State[Int, Int](i => (i*2, i*3)).map2(x)((x,y) => x + y)
    val (a, s) = instance.run(1)

    assert(a === 11)
    assert(s === 12)
  }

  test("flatMap") {
    val instance = State[Int, Int](i => (i*2, i*3)).flatMap(i => State.unit[Int, Int](i*2))
    val (a, s) = instance.run(1)

    assert(a === 4)
    assert(s === 3)
  }



}
