package fp.chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(t1, t2) => 1 + size(t1) + size(t2)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    def doWork(tree: Tree[Int], m: Int): Int = tree match {
      case Leaf(x) => m.max(x)
      case Branch(t1, t2) => doWork(t1, doWork(t2, m))
    }

    doWork(t, 0)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }


  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}