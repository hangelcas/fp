package fp.chapter2

import org.scalatest.FunSuite

class MyModuleTest extends FunSuite {


  test("fib") {
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(2) == 1)
    assert(MyModule.fib(3) == 2)
    assert(MyModule.fib(4) == 3)
    assert(MyModule.fib(5) == 5)
  }

}
