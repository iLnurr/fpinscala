package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions - not stack safe
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(_,xs) ⇒ xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil ⇒ List(h)
    case Cons(_,xs) ⇒ Cons(h,xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_,xs) if n > 0 ⇒ drop(xs, n - 1)
    case _ ⇒ l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) ⇒ dropWhile(xs,f)
    case _ ⇒ l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(_,Nil) ⇒ Nil
    case Cons(x,xs) ⇒ Cons(x,init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,len) ⇒ len + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil ⇒ z
    case Cons(x,xs) ⇒ foldLeft(xs, f(z,x))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l,Nil:List[B])((x,acc) ⇒ Cons(f(x), acc))

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) ⇒ x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)((x,y) ⇒ x * y)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x,_) ⇒ x + 1)

  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, Nil: List[A])((acc,h) ⇒ Cons(h,acc))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((acc,h) ⇒ f(h,acc)) //? foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l,z)((h,acc) ⇒ f(acc,h)) //? foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def sum4(ns: List[Int]) =
    foldLeftViaFoldRight(ns, 0)((x,y) ⇒ x + y)

  def sum5(ns: List[Int]) =
    foldRightViaFoldLeft(ns, 0)((x,y) => x + y)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1,a2)((acc,h) ⇒ Cons(h, acc))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, List[A]())((xs,acc) ⇒ append(xs,acc))

  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,acc) ⇒ if (f(h)) Cons(h,acc) else acc)

  def flatMap[A,B](as: List[A])(f: A ⇒ List[B]): List[B] =
    foldRight(as, Nil: List[B])((h,acc) ⇒ append(f(h),acc))

  def filterViaFlatMap[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    flatMap(l)(h ⇒ if (f(h)) List(h) else Nil)

  def aggregateInts(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
    case (Nil, _) ⇒ Nil
    case (_, Nil) ⇒ Nil
    case (Cons(a1,xs1), Cons(a2, xs2)) ⇒ Cons(a1 + a2, aggregateInts(xs1,xs2))
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) ⇒ C): List[C] = (l1,l2) match {
    case (Nil, _) ⇒ Nil
    case (_, Nil) ⇒ Nil
    case (Cons(a1,xs1), Cons(a2, xs2)) ⇒ Cons(f(a1,a2), zipWith(xs1,xs2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
    case (Nil,_) ⇒ false
    case (_,Nil) ⇒ false
    case (Cons(h1,_),Cons(h2,Nil)) ⇒
      h1 == h2
    case (Cons(h1,xs1),Cons(h2,xs2)) if h1 == h2 ⇒
      hasSubsequence(xs1,xs2)
    case (Cons(_,xs1),_) ⇒
      hasSubsequence(xs1, sub)
  }
}

object TestList extends App {
  import fpinscala.datastructures.List._
  val source = List(1,2,3)
  val sameList = foldRight(source, Nil:List[Int])(Cons(_,_))

  assert(source == sameList)
  assert(sum(source) == sum2(source))
  assert(sum(source) == sum3(source))
  assert(sum(source) == sum4(source))
  assert(sum(source) == sum5(source))
  assert(List.map(source)(_ + 1) == List(2,3,4)) // 3.16
  assert(List.map(source)(_.toString) == List("1","2","3")) // 3.17
  assert(List.filter(source)(_ > 1) == List(2,3),s"${List.filter(source)(_ > 1)} != ${List(2,3)}")
  assert(List.filterViaFlatMap(source)(_ > 1) == List(2,3),s"${List.filterViaFlatMap(source)(_ > 1)} != ${List(2,3)}")
  assert(aggregateInts(source, List(4,5,6)) == List(5,7,9))
  assert(zipWith(source, List(4,5,6))(_ + _) == List(5,7,9))
  assert(hasSubsequence(List(1, 2, 3, 4), List(1)))
  assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  assert(hasSubsequence(List(1, 2, 3, 4), List(2,3)))
  assert(hasSubsequence(List(1, 2, 3, 4), List(3,4)))
  assert(!hasSubsequence(List(1, 2, 3, 4), List(4)))
  assert(!hasSubsequence(List(1, 2, 3, 4), List(1, 4)))
}
