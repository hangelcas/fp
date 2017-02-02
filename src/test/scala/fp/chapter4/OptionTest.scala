package fp.chapter4

import org.scalatest.FunSuite

import scala.util.Try
import scala.{Either => _, Option => _, Some => _, _}

class OptionTest extends FunSuite {

  test("map") {
    assert((None:Option[Int]).map(_ + 1) === None)
    assert(Some(1).map(_ + 1) === Some(2))
    assert(Some("1").map(_ + 1) === Some("11"))
  }

  test("getOrElse") {
    assert((None:Option[Int]).getOrElse(1) === 1)
    assert(Some(1).getOrElse(2) === 1)
    assert(Some("1").getOrElse("2") === "1")
  }

  test("flatMap") {
    assert((None:Option[Int]).flatMap(x => Some(x + 1)) === None)
    assert(Some(1).flatMap(x => Some(x + 1)) === Some(2))
    assert(Some("1").flatMap(x => Some(x + 1)) === Some("11"))

    assert((None:Option[Int]).flatMap(_ => None) === None)
    assert(Some(1).flatMap(_ => None) === None)
    assert(Some("1").flatMap(_ => None) === None)
  }

  test("orElse") {
    assert((None:Option[Int]).orElse(Some(1)) === Some(1))
    assert((None:Option[Int]).orElse(None) === None)
    assert(Some(1).orElse(None) === Some(1))
    assert(Some(1).orElse(Some(2)) === Some(1))
    assert(Some("1").orElse(Some("2")) === Some("1"))
  }


  test("filter") {
    assert((None:Option[Int]).filter(_ == 1) === None)
    assert(Some(1).filter(_ == 1) === Some(1))
    assert(Some("1").filter(_ == 1) === None)
  }

  test("map2") {
    def f: (Int, Int) => Int = _ + _

    assert(Option.map2(None:Option[Int], None:Option[Int])(f) === None)
    assert(Option.map2(Some(2), None:Option[Int])(f) === None)
    assert(Option.map2(None:Option[Int], Some(1))(f) === None)
    assert(Option.map2(Some(2), Some(1))(f) === Some(3))
  }

  test("sequence") {
    assert(Option.sequence(List(None:Option[Int], None:Option[Int])) === None)
    assert(Option.sequence(List(Some(2), None:Option[Int])) === None)
    assert(Option.sequence(List(None:Option[Int], Some(1))) === None)
    assert(Option.sequence(List(Some(2), Some(1))) === Some(List(2,1)))
  }

  test("traverse") {
    def f: String => Option[Int] = x => try { Some(x.toInt) } catch { case _ => None }

    assert(Option.traverse(List("1"))(f) === Some(List(1)))
    assert(Option.traverse(List("1", "2"))(f) === Some(List(1, 2)))
    assert(Option.traverse(List("1", "two"))(f) === None)
  }
}
