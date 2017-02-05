package fp.chapter5

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    this match {
      case Empty => this
      case Cons(h, t) if (n <= 0) => Empty
      case Cons(h, t) =>  cons(h(), t().take(n-1))

    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if (n > 0) => t().drop(n-1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => this
      case Cons(h, _) if (!p(h())) => Empty
      case Cons(h, t) => Cons(h, () => t().takeWhile(p))
    }
  }


  def takeWhile_2(p: A => Boolean): Stream[A] = {
    foldRight(Empty:Stream[A]) ( (a, acc) =>
      if (!p(a)) acc else Cons(() => a, () => acc)
    )
  }


  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) => p(h()) &&  t().forAll(p)
    }
  }

  def headOption: Option[A] = {
    foldRight(None:Option[A]) { (a, acc) =>
      Some(a)
    }
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.


  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B]) { (a, acc) =>
      cons(f(a), acc)
    }
  }

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h,t) => f(h) append t)
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { (a, acc) =>
      if(f(a)) cons(a, acc) else acc
    }
  }

  def append[B >: A](f: => Stream[B]): Stream[B] = {
    foldRight(f) { (a, acc) =>
      cons(a, acc)
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = {
    foldRight(List.empty[A]) { (a, acc) =>
      a :: acc
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map {
      case (a, s) => cons(a, unfold(s)(f))
    }.getOrElse(empty)
  }
}