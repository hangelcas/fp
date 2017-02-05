package fp.chapter4

import org.scalatest.FunSuite

import scala.{Option => _, Either => _, Left => _, Right => _, _}

class EitherTest extends FunSuite {

  val ex = new Exception("@@@")

  test("map") {
    assert(Left(ex).map(_ => 1) === Left(ex))
    assert(Right("1").map(_.toInt) === Right(1))
  }

  test("flatMap") {
    assert(Left(ex).flatMap(_ => Right(1)) === Left(ex))
    assert(Right(2).flatMap(_ => Right(1)) === Right(1))
    assert(Right(2).flatMap(_ => Left(ex)) === Left(ex))
  }

  test("orElse") {
    assert(Left(ex).orElse(Right(1)) === Right(1))
    assert(Right(2).orElse(Right(1)) === Right(2))
  }

  test("map2") {
    assert(Left(ex).map2(Left(ex))((_, _) => 1) === Left(ex))
    assert(Left(ex).map2(Right(1))((_, _) => 2) === Left(ex))
    assert(Right(1).map2(Left(ex))((_, _) => 2) === Left(ex))
    assert(Right(1).map2(Right(2))((_, _) => 3) === Right(3))
  }
}
