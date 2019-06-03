package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa,fab){case (a,aToB) => aToB(a)}

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List.empty[A])){ case (acc,fa) => map2(fa,acc)(_ :: _)}

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])){ case (a,acc) => map2(f(a),acc)(_ :: _)}

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def map3[A,B,C,D](fa: F[A], fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(
      apply(
        map(fa)(f.curried)
      )(fb)
    )(fc)
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(
      apply(
        apply(
          map(fa)(f.curried)
        )(fb)
      )(fc)
    )(fd)


  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa,fb){ case (a,b) => a -> b}

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a),G.unit(a))
    override def map2[A, B, C](f1: (F[A], G[A]), f2: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
      val (fa,ga) = f1
      val (fb,gb) = f2
      self.map2(fa,fb)(f) -> G.map2(ga,gb)(f)
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] =
      self.unit(G.unit(a))
    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fa,fb)(G.map2(_,_)(f))
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map.empty[K,V])){ case ((k,fv),fmap) => map2(fv,fmap){case (v,m) => m.updated(k,v)}}
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] { self =>
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =  ma match {
      case Right(value) => f(value)
      case Left(value) => Left(value)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???

  type Id[A] = A

  val identityMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: A)(f: A => Id[B]): Id[B] = f(a)
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa,fb) match {
      case (Success(a),Success(b)) => Success(f(a,b))
      case (Failure(ea,eva), Failure(eb,evb)) => Failure(eb, (ea +: eva) ++ evb)
      case (Failure(ea,eva), Success(_)) => Failure(ea, eva)
      case (Success(_), Failure(eb,evb)) => Failure(eb, evb)
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Monad.Id,A,B](fa)(f)(Monad.identityMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_,as) => as.head -> as.tail)._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa,z)((a,b) => () -> f(b,a))._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    val productApplicative: Applicative[({type f[x] = (G[x], H[x])})#f] = G.product(H)
    traverse[({type f[x] = (G[x], H[x])})#f,A,B](fa)(a => (f(a),g(a)))(productApplicative)
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
      self.traverse(fa)(ga => G.traverse(ga)(f))
  }

}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit ga: Applicative[G]): G[List[B]] =
      fa.foldRight(ga.unit(List.empty[B])){case (a,acc) => ga.map2(f(a),acc)(_ :: _)}
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit ga: Applicative[G]): G[Option[B]] = fa match {
      case Some(a) => ga.map(f(a))(b => Option(b))
      case None => ga.unit(None)
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit ga: Applicative[G]): G[Tree[B]] =
      ga.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

  def composeM[F[_],G[_]](FM: Monad[F], GM: Monad[G], GT: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        FM.unit(GM.unit(a))
      override def flatMap[A, B](fga: F[G[A]])(a_TO_fgb: A => F[G[B]]): F[G[B]] =
        FM.flatMap(fga){ ga =>
          val fggb: F[G[G[B]]] = GT.traverse(ga)(a_TO_fgb)(FM)
          val fgb: F[G[B]] = FM.map(fggb)(GM.join)
          fgb
        }
    }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
