package fp.chapter6

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, n) = rng.nextInt
    val r = Math.abs(if (i == Int.MinValue) i-1 else i)
    (r, n)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, n) = nonNegativeInt(rng)
    if (i == Int.MaxValue) {
      (0, n)
    } else {
      (i.toDouble / Int.MaxValue, n)
    }
  }


  def double_map: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, n1) = nonNegativeInt(rng)
    val (d, n2) = double(n1)
    ((i,d), n2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), n) = intDouble(rng)
    ((d, i), n)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((List.empty[Int], rng)) {
      case ((accu, rng), _) =>
        val (i, n) = rng.nextInt
        ((i :: accu), n)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      (f(a, b), r3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft((List.empty[A], rng)) {
        case ((accu, rn), f) =>
          val (i, r) = f.apply(rn)
          (i :: accu, r)
      }
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
//    State {
//      s1 => {
//        val (a, s2) = run(s1)
//        (f(a), s2)
//      }
//    }
    flatMap(a => State.unit(f(a)))
  }


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
//    State {
//      s1 => {
//        val (a, s2) = run(s1)
//        val (b, s3) = sb.run(s2)
//        (f(a, b), s3)
//      }
//    }
    flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State {
      s1 => {
        val (a, s2) = run(s1)
        f(a).run(s2)
      }
    }
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
