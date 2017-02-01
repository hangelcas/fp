package fp.chapter4

import org.scalatest.FunSuite
import scala.{Option => _, Some => _, Either => _, _}

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
}
