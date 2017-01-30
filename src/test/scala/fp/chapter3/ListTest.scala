package fp.chapter3

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("tail") {
    assert(List.tail(Nil) === Nil)
    assert(List.tail(List(1)) === Nil)
    assert(List.tail(List(1, 2)) === Cons(2, Nil))
    assert(List.tail(List(1, 2, 3)) === Cons(2, Cons(3, Nil)))
  }


  test("setHead") {
    assert(List.setHead(Nil, 0) === Cons(0, Nil))
    assert(List.setHead(List(1), 0) === Cons(0, Nil))
    assert(List.setHead(List(1, 2), 0) === Cons(0, Cons(2, Nil)))
  }

  test("drop") {
    assert(List.drop(Nil, 2) === Nil)
    assert(List.drop(List(1), 1) === Nil)
    assert(List.drop(List(1), 2) === Nil)
    assert(List.drop(List(1, 2), 1) === Cons(2, Nil))
    assert(List.drop(List(1, 2), 2) === Nil)
    assert(List.drop(List(1, 2, 3), 2) === Cons(3, Nil))
    assert(List.drop(List(1, 2, 3), 5) === Nil)
  }

  test("dropWhile") {
    def is(i: Int): Int => Boolean = _ == i
    assert(List.dropWhile(Nil, is(1)) === Nil)
    assert(List.dropWhile(List(1), is(1)) === Nil)
    assert(List.dropWhile(List(1), is(2)) === Cons(1, Nil))
    assert(List.dropWhile(List(1, 2), is(1)) === Cons(2, Nil))
    assert(List.dropWhile(List(1, 2), is(2)) === Cons(1, Cons(2, Nil)))
  }

  test("init") {
    assert(List.init(Nil) === Nil)
    assert(List.init(List(1)) === Nil)
    assert(List.init(List(1, 2)) === Cons(1, Nil))
    assert(List.init(List(1, 2, 3)) === Cons(1, Cons(2, Nil)))
  }

  test("length") {
    assert(List.length(Nil) === 0)
    assert(List.length(List(1)) === 1)
    assert(List.length(List(1, 2)) === 2)
    assert(List.length(List(1, 2, 3)) === 3)
  }

  test("foldLeft") {
    assert(List.foldLeft(Nil, 0)((b, _) => b + 1) === 0)
    assert(List.foldLeft(List(1), 0)((b, _) => b + 1) === 1)
    assert(List.foldLeft(List(1, 2), 0)((b, _) => b + 1) === 2)
  }

  test("sumFL") {
    assert(List.sumFL(Nil) === 0)
    assert(List.sumFL(List(1)) === 1)
    assert(List.sumFL(List(1, 2)) === 3)
    assert(List.sumFL(List(1, 2, 3)) === 6)
  }

  test("productFL") {
    assert(List.sumFL(Nil) === 0)
    assert(List.sumFL(List(1)) === 1)
    assert(List.sumFL(List(2, 2, 2)) === 6)
  }

  test("lengthFL") {
    assert(List.lengthFL(Nil) === 0)
    assert(List.lengthFL(List(1)) === 1)
    assert(List.lengthFL(List(1, 2)) === 2)
    assert(List.lengthFL(List(1, 2, 3)) === 3)
  }

  test("reverse") {
    assert(List.reverse(Nil) === Nil)
    assert(List.reverse(List(1)) === List(1))
    assert(List.reverse(List(1, 2)) === List(2, 1))
    assert(List.reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  test("appendFold") {
    assert(List.appendFold(Nil, Nil) === Nil)
    assert(List.appendFold(List(1), Nil) === List(1))
    assert(List.appendFold(Nil, List(1)) === List(1))
    assert(List.appendFold(List(1, 2), List(3,4)) === List(1, 2, 3, 4))
  }

  test("concatenate") {
    assert(List.concatenate(List(Nil)) === Nil)
    assert(List.concatenate(List(Nil, Nil)) === Nil)
    assert(List.concatenate(List(List(1), Nil)) === List(1))
    assert(List.concatenate(List(Nil, List(1))) === List(1))
    assert(List.concatenate(List(List(1), List(2))) === List(1, 2))
    assert(List.concatenate(List(List(1), List(2, 3))) === List(1, 2, 3))

    assert(List.concatenate(List(List(1, 1), List(2, 2), List(3, 3))) === List(1, 1, 2, 2, 3, 3))
  }

  test("addOne") {
    assert(List.addOne(Nil) === Nil)
    assert(List.addOne(List(1)) === List(2))
    assert(List.addOne(List(1, 2)) === List(2, 3))
    assert(List.addOne(List(1, 2, 3)) === List(2, 3, 4))
  }


  test("turnToString") {
    assert(List.turnToString(Nil) === Nil)
    assert(List.turnToString(List(1.0)) === List("1.0"))
    assert(List.turnToString(List(1.0, 2.0)) === List("1.0", "2.0"))
    assert(List.turnToString(List(1.0, 2.0, 3.0)) === List("1.0", "2.0", "3.0"))
  }


  test("filter") {
    def isEven(i: Int) = i % 2 == 0
    assert(List.filter(Nil)(isEven) === Nil)
    assert(List.filter(List(1))(isEven) === Nil)
    assert(List.filter(List(1, 2))(isEven) === List(2))
    assert(List.filter(List(1, 2, 3))(isEven) === List(2))
  }

  test("flatMap") {
    assert(List.flatMap(Nil)(i => List(i,i)) === Nil)
    assert(List.flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3))
  }

  test("zipWith") {
    assert(List.zipWith(List(1,2,3), List(4,5,6))(_ + _) === List(5,7,9))
  }


}
