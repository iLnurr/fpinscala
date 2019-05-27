package fpinscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                               f: (A,B) => C) extends Future[C] {
    val cache: AtomicReference[Option[C]] = new AtomicReference[Option[C]](None)
    def isDone = cache.get().isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache.get() match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache.set(Some(ret))
        ret
    }
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      Map2Future(af,bf,f)
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = (es: ExecutorService) => {
    val a = run(es)(p).get() // blocking
    f(a)(es)
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequenceFR[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](Par.unit(List.empty[A]))((p,acc) ⇒ map2(p,acc)(_ :: _))

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parFilter[A](as: List[A])(f: A ⇒ Boolean): Par[List[A]] = fork {
    val filtered = as.map(asyncF(Some(_).filter(f)))
    map(sequence(filtered))(_.flatten)
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFlat[A](as: Seq[A], default: A)(f: (A,A) ⇒ A): Par[A] =
    if (as.size <= 1) {
        unit(as.headOption.getOrElse(default))
    } else {
      val (l,r) = as.splitAt(as.length/2)
      map2(parFlat(l,default)(f),parFlat(r,default)(f))(f)
    }


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
  }
}

object Examples extends App {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  import Par._
  def max(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption getOrElse Int.MinValue
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      math.max(max(l),max(r))
    }
  }

  def maxPar(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      unit(ints.headOption getOrElse Int.MinValue)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      map2(maxPar(l),maxPar(r))(math.max)
    }
  }

  def maxParFlat(ints: IndexedSeq[Int]): Par[Int] =
    parFlat(ints,Int.MinValue)(math.max)

  def sumPF(ints: IndexedSeq[Int]): Par[Int] =
    parFlat(ints,0)(_ + _)

  val l = List(1,2,3,4,5)

  assert(max(l.toIndexedSeq) == 5)

  val es = Executors.newCachedThreadPool()
  assert(run(es)(maxPar(l.toIndexedSeq)).get() == 5)
  assert(run(es)(maxParFlat(l.toIndexedSeq)).get() == 5)
  assert(run(es)(sumPF(l.toIndexedSeq)).get() == 15)

}
