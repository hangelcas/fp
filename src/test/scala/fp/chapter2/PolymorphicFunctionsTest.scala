package fp.chapter2

import org.scalatest.FunSuite

class PolymorphicFunctionsTest extends FunSuite {

  test("Sorted") {
    val input = Array(1,2,3,4,5)
    val greaterThan = (a: Int, b: Int) => a < b
    assert(PolymorphicFunctions.isSorted(input, greaterThan))
  }


  test("Not Sorted") {
    val input = Array(3,2,1,4,5)
    val greaterThan = (a: Int, b: Int) => a < b
    assert(PolymorphicFunctions.isSorted(input, greaterThan) === false)
  }


  test("Not Sorted reversed") {
    val input = Array(1,2,3,4,5).reverse
    val greaterThan = (a: Int, b: Int) => a < b
    assert(PolymorphicFunctions.isSorted(input, greaterThan) === false)
  }

}
