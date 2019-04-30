package fpinscala.state

import fpinscala.gettingstarted.PolymorphicFunctions._
import fpinscala.state.RNG.{Simple, nonNegativeLessThan}


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

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    map(_.nextInt)(x ⇒ if (x < 0) - x + 1 else x)(rng)

  def double(rng: RNG): (Double, RNG) =
    map(_.nextInt)(_.toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intR,rng1) = rng.nextInt
    val (doubleR,rng2) = double(rng1)
    ((intR,doubleR),rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) {
      List() → rng
    } else {
      (1 to count).foldRight((List.empty[Int],rng)){ case (_,(list,rngg)) ⇒
        val (i,r) = rngg.nextInt
        (i :: list, r)
      }
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng ⇒ {
    val (a,rnga) = ra(rng)
    val (b,rngb) = rb(rnga)
    f(a,b) → rngb
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng ⇒ {
    fs.foldRight(List.empty[A] → rng){case (a,(list,rngacc)) ⇒
      map(a)(_ :: list)(rngacc)
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])){case (randA,randListA) ⇒
      map2(randA,randListA)(_ :: _)
    }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng ⇒ {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFM[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a ⇒ unit(f(a)))

  def map2ViaFM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s ⇒ {
    val (a,ss) = run(s)
    f(a) → ss
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s ⇒ {
    val (a,saa) = run(s)
    val (b,sbb) = sb.run(saa)
    f(a,b) → sbb
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s ⇒ {
    val (a,ss) = run(s)
    f(a).run(ss)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
    State((a,_))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((stateA,stateList) ⇒ stateA.map2(stateList)(_ :: _))

  def updateMachine: Input ⇒ Machine ⇒ Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int,Int)] = {
    inputs
      .foldRight(State[Machine,Machine](m ⇒ (m,m)))((in,acc) ⇒ acc.map(m ⇒ updateMachine(in)(m)))
      .map(m ⇒ (m.coins, m.candies))
  }
}

object TestState extends App {
  val (result1, rng1) = nonNegativeLessThan(10)(Simple(47))
  val result2 = nonNegativeLessThan(10)(rng1)._1

  assert(result1 >= 0)
  assert(result1 < 10)
  assert(result2 >= 0)
  assert(result2 < 10)
  assert(result1 != result2)
}
