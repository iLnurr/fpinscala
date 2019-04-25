package fpinscala.laziness

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
  def toList: List[A] = this match {//foldRight(List.empty[A])(_ :: _)
    case Cons(h,t) ⇒ h() :: t().toList
    case _ ⇒ List()
  }

  def take(n: Int): Stream[A] = this match { // foldRight((n,Stream.empty[A]))((a,acc) ⇒ if (acc._1 > 0) (n-1,Stream.cons(a, acc._2)) else acc)._2
    case Cons(h,t) if n > 1 ⇒ cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 ⇒ cons(h(), empty)
    case _ ⇒ empty
  }

  def takeUF(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t),nn) if nn > 1 ⇒ Some((h(),(t(),nn-1)))
      case (Cons(h,_),1) ⇒ Some((h(),(empty,0)))
      case _ ⇒ None
    }

  def drop(n: Int): Stream[A] = foldRight(empty[A])((_,t) ⇒ if (n > 1) t.drop(n-1) else t)

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) ⇒ cons(h(), t().takeWhile(p))
    case _ ⇒ empty
  }

  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) ⇒ if (p(h)) cons(h,t) else empty)

  def takeWhileUF(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) ⇒ Some(h(),t())
      case _ ⇒ None
    }

  def zipWith[B,C](l2: Stream[B])(f: (A,B) ⇒ C): Stream[C] =
    unfold(this → l2) {
      case (Empty, _) ⇒ None
      case (_, Empty) ⇒ None
      case (Cons(a, aa), Cons(b, bb)) ⇒ Some(f(a(),b()), aa() → bb())
    }

  /**
    * The zipAll function should continue the traversal as long as either stream has more elements—
    * it uses Option to indicate whether each stream has been exhausted.
    */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this → s2) {
      case (Empty,Empty) ⇒ None
      case (Empty, Cons(b, bb)) ⇒ Some((Option.empty[A] → Some(b()), Empty → bb()))
      case (Cons(a, aa), Empty) ⇒ Some((Some(a()) → Option.empty[B], aa() → Empty))
      case (Cons(a, aa), Cons(b, bb)) ⇒ Some((Some(a()) → Some(b()), aa() → bb()))
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((a,b) ⇒ p(a) && b)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((h,_) ⇒ Option(h))

  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a,b) ⇒ cons(f(a),b))

  def mapUF[B](f: A ⇒ B): Stream[B] =
    unfold(this) {
      case Cons(h, t) ⇒ Some((f(h), t()))
      case _ ⇒ None
    }

  def filter(f: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((h,t) ⇒ if(f(h)) cons(h,t) else t)

  def append[B >: A](s2: ⇒ Stream[B]): Stream[B] =
    foldRight(s2)((h,t) ⇒ cons(h, t))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) ⇒ f(h).append(t))


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).filter(_._2.isDefined).forAll{case (a,b) ⇒ a == b}

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty ⇒ None
      case Cons(h,t) ⇒ Some((cons(h(),t()),t()))
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z,Stream(z))){ case (a,(zz,streamz)) ⇒
      val b = f(a,zz)  // eager evaluation of zz!
      b → cons(b,streamz)
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a)) //dummy

  def from(n: Int): Stream[Int] =
    cons(n,from(n + 1))

  def fibs: Stream[Int] = {
    def go(f: Int, s: Int): Stream[Int] = cons(f, go(s, f + s))
    go(0,1)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match { // corecursive,guarded recursion - ag produce data
    case None ⇒ empty[A]
    case Some((a,s)) ⇒ cons(a, unfold(s)(f))
  }

  def fubsUF: Stream[Int] =
    unfold(0 → 1){case (s1,s2) ⇒ Some(s1 → (s2,s1 + s2)) }

  def fromUF(n: Int): Stream[Int] =
    unfold(n)(s ⇒ Some(s,s + 1))

  def constantUF[A](a: A): Stream[A] =
    unfold(a)(_ ⇒ Some(a,a))

  val onesUF: Stream[Int] = unfold(1)(_ ⇒ Some(1,1))

}