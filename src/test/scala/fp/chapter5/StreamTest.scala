package fp.chapter5

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("toList") {
    assert(Empty.toList === Nil)
    assert(Stream.apply(1,2,3).toList === List(1,2,3))
  }

  test("take") {
    assert(Empty.take(2) === Empty)
    assert(Stream.apply(1,2,3).take(2).toList === List(1,2))
    assert(Stream.apply(1,2,3).take(0) === Empty)
  }

  test("take_unfold") {
    assert(Empty.take_unfold(2) === Empty)
    assert(Stream.apply(1,2,3).take_unfold(2).toList === List(1,2))
    assert(Stream.apply(1,2,3).take_unfold(0) === Empty)
  }


  test("drop") {
    assert(Empty.drop(2) === Empty)
    assert(Stream.apply(1,2,3).drop(2).toList === List(3))
    assert(Stream.apply(1,2,3).drop(0).toList === List(1,2,3))
    assert(Stream.apply(1,2,3).drop(3) === Empty)
    assert(Stream.apply(1,2,3).drop(4) === Empty)
  }

  test("takeWhile") {
    assert(Empty.takeWhile(_ => true) === Empty)
    assert(Stream.apply(1,2,3).takeWhile(_ == 1).toList === List(1))
    assert(Stream.apply(1,2,3).takeWhile(_ < 3).toList === List(1,2))
    assert(Stream.apply(1,2,3).takeWhile(_ => true).toList === List(1,2,3))
  }

  test("takeWhile_2") {
    assert(Empty.takeWhile_2(_ => true) === Empty)
    assert(Stream.apply(1,2,3).takeWhile_2(_ == 1).toList === List(1))
    assert(Stream.apply(1,2,3).takeWhile_2(_ < 3).toList === List(1,2))
    assert(Stream.apply(1,2,3).takeWhile_2(_ => true).toList === List(1,2,3))
  }


  test("takeWhile_unfold") {
    assert(Empty.takeWhile_unfold(_ => true) === Empty)
    assert(Stream.apply(1,2,3).takeWhile_unfold(_ == 1).toList === List(1))
    assert(Stream.apply(1,2,3).takeWhile_unfold(_ < 3).toList === List(1,2))
    assert(Stream.apply(1,2,3).takeWhile_unfold(_ => true).toList === List(1,2,3))
  }

  test("forAll") {
    assert(Empty.forAll(_ => true) === true)
    assert(Empty.forAll(_ => false) === true)
    assert(Stream.apply(1,2,3).forAll(_ => true) === true)
    assert(Stream.apply(1,2,3).forAll(_ => false) === false)
    assert(Stream.apply(1,2,3).forAll(_ > 3) === false)
    assert(Stream.apply(1,2,3).forAll(_ <= 3) === true)
  }


  test("headOption") {
    assert(Empty.headOption == None)
    assert(Stream.apply(1,2).headOption == Some(1))
  }


  test("map") {
    assert(Stream.empty[Int].map((x: Int) => x * 2) === Empty)
    assert(Stream.apply(1,2,3).map((x: Int) => x * 2).toList === List(2,4,6))
  }

  test("map_unfold") {
    assert(Stream.empty[Int].map_unfold((x: Int) => x * 2) === Empty)
    assert(Stream.apply(1,2,3).map_unfold((x: Int) => x * 2).toList === List(2,4,6))
  }

  test("filter") {
    assert(Stream.empty[Int].filter(_ > 2) === Empty)
    assert(Stream.apply(1,2,3).filter(_ > 1).toList === List(2,3))
  }

  test("append") {
    assert(Stream.empty[Int].append(Stream.empty[Int]) === Stream.empty[Int])
    assert(Stream.empty[Int].append(Stream(1,2)).toList === List(1,2))
    assert(Stream(3,4).append(Stream(1,2)).toList === List(3,4,1,2))
  }


  test("flatMap") {
    assert(Stream.empty[Int].flatMap(_ => Stream.empty[Int]) === Stream.empty[Int])
    assert(Stream.empty[Int].flatMap(_ => Stream(1,2)) === Empty)
    assert(Stream(1,2).flatMap(x => Stream(x*2,x*3)).toList === List(2,3,4,6))
  }

  test("constant") {
    assert(Stream.constant(3).take(3).toList === List(3,3,3))
  }

  test("constant_unfold") {
    assert(Stream.constant_unfold(3).take(3).toList === List(3,3,3))
  }

  test("from") {
    assert(Stream.from(5).take(3).toList === List(5,6,7))
  }


  test("ones") {
    assert(Stream.ones_unfold.take(3).toList === List(1,1,1))
  }

  test("from_unfold") {
    assert(Stream.from_unfold(5).take(3).toList === List(6,7,8))
  }

  test("unfold") {
    assert(Stream.unfold(5)(x => Some(x+1, x+1)).take(3).toList === List(6,7,8))
  }

  test("zipWith") {
    assert(Stream.empty[Int].zipAll(Stream(1)).toList === List((None, Some(1))))
    assert(Stream(1).zipAll(Stream.empty[Int]).toList === List((Some(1), None)))
    assert(Stream(1).zipAll(Stream(2)).toList === List((Some(1), Some(2))))
  }

  test("startsWith") {
    assert(Stream(1,2,3).startsWith(Stream(1,2)) === true)
  }
}
