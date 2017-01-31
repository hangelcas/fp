package fp.chapter4

import org.scalatest.FunSuite
import scala.{Option => _, Some => _, Either => _, _}

class OptionTest extends FunSuite {

  test("map") {
    assert((None:Option[Int]).map(_ + 1) === None)
    assert(Some(1).map(_ + 1) === Some(2))
    assert(Some("1").map(_ + 1) === Some("11"))
  }

  test("getOrELse") {
    assert((None:Option[Int]).getOrElse(1) === 1)
    assert(Some(1).getOrElse(2) === 1)
    assert(Some("1").getOrElse("2") === "1")
  }
}
