package fp.chapter3

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("size") {
    assert(Tree.size(Leaf(1)) === 1)
    assert(Tree.size(Branch(Leaf(1), Leaf(2))) === 3)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2))) === 5)
    assert(Tree.size(Branch(Leaf(2), Branch(Leaf(1), Leaf(1)))) === 5)
  }

  test("max") {
    assert(Tree.maximum(Leaf(1)) === 1)
    assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) === 2)
    assert(Tree.maximum(Branch(Leaf(2), Leaf(1))) === 2)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2))) === 2)
    assert(Tree.maximum(Branch(Leaf(2), Branch(Leaf(1), Leaf(1)))) === 2)
  }


  test("depth") {
    assert(Tree.maximum(Leaf(1)) === 1)
    assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) === 2)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2))) === 3)
  }


  test("map") {
    def double(i: Int) = i * 2

    assert(Tree.map(Leaf(1))(double) === Leaf(2))
    assert(Tree.map(Branch(Leaf(1), Leaf(2)))(double) === Branch(Leaf(2), Leaf(4)))
    assert(Tree.map(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2)))(double) === Branch(Branch(Leaf(2), Leaf(6)), Leaf(4)))
  }

}
