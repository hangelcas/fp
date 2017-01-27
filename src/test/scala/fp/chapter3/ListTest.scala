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
}