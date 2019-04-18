package fpinscala.datastructures

import scala.annotation.tailrec
import math._

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size(t: Tree[_]): Int = t match { // not stack safe!
    case Leaf(_) ⇒ 1
    case Branch(left, right) ⇒ 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match { // not stack safe!
    case Leaf(v) ⇒ v
    case Branch(l,r) ⇒ max(maximum(l),maximum(r))
  }

  def depth(t: Tree[_]): Int = t match {
    case Leaf(_) ⇒ 0
    case Branch(l,r) ⇒1 + max(depth(l),depth(r))
  }

  def map[A,B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(a) ⇒ Leaf(f(a))
    case Branch(l,r) ⇒ Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A ⇒ B)(aggr: (B,B) ⇒ B): B = t match {
    case Leaf(a) ⇒ f(a)
    case Branch(l,r) ⇒ aggr(fold(l)(f)(aggr), fold(r)(f)(aggr))
  }

  def size2(t: Tree[_]): Int =
    fold(t)(_ ⇒ 1)((ls,rs) ⇒ 1 + ls + rs)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(identity)(max)

  def depth2(t: Tree[_]): Int =
    fold(t)(_ ⇒ 0)((ls,rs) ⇒ 1 + max(ls,rs))

  def map2[A,B](t: Tree[A])(f: A ⇒ B): Tree[B] =
    fold[A, Tree[B]](t)(a ⇒ Leaf(f(a)))(Branch(_,_))


}

object TestTree extends App {
  import Tree._
  def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  assert(size(t) == 5)
  assert(maximum(t) == 3)
  assert(depth(t) == 2, s"${depth(t)} != 2")
  assert(maximum(map(t)(x ⇒ x * 2)) == 6)

  assert(size2(t) == 5,s"${size2(t)} != 5")
  assert(maximum2(t) == 3)
  assert(depth2(t) == 2, s"${depth2(t)} != 2")
  assert(maximum(map2(t)(x ⇒ x * 2)) == 6)
}